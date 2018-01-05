(** © Jeremy Yallop, 2017-2018 *)

type (_,_) fmt =
  | Int : (int -> 'a, 'a) fmt
  | Lit : string -> ('a, 'a) fmt
  | Bool : (bool -> 'a, 'a) fmt
  | Cat : ('a, 'b) fmt * ('b, 'c) fmt -> ('a, 'c) fmt
let (%) x y = Cat (x, y)

(* Unstaged printf.  The auxiliary printk function is in CPS and
   the interface fucntion sprintf supplies the top-level continuation.
*)
let rec printk : type a r. (string -> r) -> (a, r) fmt -> a =
  fun k fmt -> match fmt with
  | Int -> fun i -> (k (string_of_int i))
  | Bool -> fun b -> (k (string_of_bool b))
  | Lit s -> k s
  | Cat (l, r) ->
     printk (fun x ->
     printk (fun y ->
       k  (x ^ y)) 
       r)
       l

let sprintf : type a. (a, string) fmt -> a =
  fun fmt -> printk (fun x -> x) fmt 

(* Naively-staged printf, identical to printk/sprintf except for
   staging annotations.
*)
macro rec printk2 : type a r. (string expr -> r expr) -> (a, r) fmt -> a expr =
  fun k fmt -> match fmt with
    | Int -> << fun i -> $(k <<string_of_int i>>) >>
    | Bool -> << fun b -> $(k <<string_of_bool b>>) >>
    | Lit s -> k (Expr.of_string s)
    | Cat (l, r) ->
      printk2 (fun x ->
      printk2 (fun y ->
      k << $x ^ $y >>) 
        r)
        l

macro sprintf2 : type a. (a, string) fmt -> a expr =
 fun fmt -> printk2 (fun x -> x) fmt 

(* Partially-static strings with append.
*)
static module Ps_string :
sig
  type t
  val sta : string -> t
  val dyn : string expr -> t
  val cd : t -> string expr
  val (++) : t -> t -> t
end =
struct
  open ~Pervasives
  type t =
      One of string option
    | Cons of string option * string expr * t

  let (+?) l r = match l, r with
      None, s | s, None -> s
    | Some a, Some b -> Some (a ^ b)

  let rec (++) l r = match l, r with
    | One l, One r -> One (l +? r)
    | One a, Cons (b, c, d) -> Cons (a +? b, c, d)
    | Cons (a, b, c), e -> Cons (a, b, c ++ e)

  let sta s = One (Some s)
  let dyn d = Cons (None, d, One None)

  let cd_optl s r = match s with
      None -> r
    | Some c -> let open Pervasives in << $(Expr.of_string c) ^ $r >>

  let cd_optr r s = match s with
      None -> r
    | Some c -> Pervasives.(<< $r ^ $(Expr.of_string c) >>)

  let rec cd = function
    | One None -> << "" >>
    | One (Some s) -> Expr.of_string s
    | Cons (o, d, One o2) -> cd_optl o (cd_optr d o2)
    | Cons (o, d, c) -> Pervasives.(<< $(cd_optl o d) ^ $(cd c) >>)
end

(* An improved staged printf based on partially-static strings.
   For some format strings this generates code with fewer
   catenations.
*)
macro rec printk3 : type a r. (Ps_string.t -> r expr) -> (a, r) fmt -> a expr =
  fun k fmt -> let open Ps_string in match fmt with
    | Int -> << fun i -> $(k (dyn <<string_of_int i>>)) >>
    | Bool -> << fun b -> $(k (dyn <<string_of_bool b>>)) >>
    | Lit s -> k (sta s)
    | Cat (l, r) ->
      printk3 (fun x ->
      printk3 (fun y ->
      k (x ++ y)) 
        r)
        l

macro sprintf3 : type a. (a, string) fmt -> a expr =
  fun fmt -> printk3 (fun x -> Ps_string.cd x) fmt 

(* An interface to if insertion.
   
   The if_locus function marks a point where an 'if' can be inserted
   into the generated code.

   The split function turns a dynamic bool into a static bool by
   running the continuation between 'split' and 'if_locus' twice,
   passing in 'true' and 'false'.

   This is a variant of The Trick (bounded static variation).
*)
type split_result =
    Done
  | Split of bool expr * (bool -> split_result)

static if_locus_prompt = ~Delimcc.new_prompt ()

static split : type a. bool expr -> bool
 = fun b -> ~Delimcc.shift0 if_locus_prompt (fun k -> Split (b, k))

macro if_locus : type a. (unit -> a expr) -> a expr =
  fun f ->
    let open ~Pervasives in
    let r = ref (fun _ -> assert false) in
    let rec handle = function
      | Done -> !r ()
      | Split (b, k) -> << if $(b) then $(handle (k true))
                           else $(handle (k false)) >>
    in 
    handle @@ ~Delimcc.push_prompt if_locus_prompt @@ fun () ->
    let x = f () in
    begin
      (r := fun () -> x);
      Done
    end


(* An interface to let insertion.

   The let_locus function marks a point where a 'let' can be inserted
   into the generated code.

   The 'genlet' function inserts a let binding for its expression
   argument at the point where let_locus was called and returns
   the bound variable.

   The 'genlet2' function is a variant of 'genlet' where the
   expression argument has pair type.  The inserted let binding
   projects the two components of the pair, and 'genlet2' returns
   the resulting two bound variables.
*)
type let_result =
    Done
  | GenLet : 'a expr * ('a expr -> let_result) -> let_result
  | GenLet2 : ('a * 'b) expr * ('a expr * 'b expr -> let_result) -> let_result

static let_locus_prompt = ~Delimcc.new_prompt ()

static genlet : type a. a expr -> a expr
 = fun v -> ~Delimcc.shift0 let_locus_prompt (fun k -> GenLet (v, k))

static genlet2 : type a b. (a * b) expr -> a expr * b expr
 = fun v -> ~Delimcc.shift0 let_locus_prompt (fun k -> GenLet2 (v, k))

macro let_locus : type a. (unit -> a expr) -> a expr =
  fun f ->
    let open ~Pervasives in
    let r = ref (fun _ -> assert false) in
    let rec handle = function
      | Done -> !r ()
      | GenLet (v, k) -> << let x = $(v) in $(handle (k <<x>>)) >>
      | GenLet2 (p, k) -> << let x, y = $(p) in $(handle (k (<<x>>, <<y>>))) >>
    in 
    handle @@ ~Delimcc.push_prompt let_locus_prompt @@ fun () ->
    let x = f () in
    begin
      (r := fun () -> x);
      Done
    end

(* An improved staged printf based on partially-static strings,
   let-insertion and if insertion.

   For a format string built from Cat, Bool and Lit this generates
   code with no catenations, even thought the values of the bool
   arguments are not known until runtime.
*)
macro rec printk4 : type a r. (Ps_string.t -> r expr) -> (a, r) fmt -> a expr =
  fun k fmt -> let open Ps_string in match fmt with
    | Int -> genlet << fun i -> $(k (dyn <<string_of_int i>>)) >>
    | Bool -> genlet << fun b -> $(if_locus (fun () ->
      match split <<b>> with
        true -> k (sta "true")
      | false -> k (sta "false"))) >>
    | Lit s -> k (sta s)
    | Cat (l, r) ->
      printk4 (fun x ->
      printk4 (fun y ->
      k (x ++ y)) 
        r)
        l

macro sprintf4 : type a. (a, string) fmt -> a expr =
  fun fmt -> let_locus (fun () -> printk4 Ps_string.cd fmt)

(* Various auxiliary functions for scanf: read_int, read_exact,
   read_bool *)
type 'a reader = string -> 'a * string

let is_digit c = String.contains "0123456789" c

let read_int : int reader =
  fun s ->
    let i = ref 0 in
    let slen = String.length s in
    while !i < slen && is_digit s.[!i] do incr i; done;
    match !i with
      0 -> failwith "read_int"
    | n ->
       let int = String.sub s 0 !i in
       let rest = String.sub s !i (slen - !i) in
       (int_of_string int, rest)

let read_exact : string -> unit reader =
  fun e s ->
    let slen = String.length s
    and elen = String.length e in
    if slen >= elen && e = String.sub s 0 elen then
      ((), String.sub s elen (slen - elen))
    else failwith "read_exact" 

let read_bool : bool reader =
  fun s ->
    let slen = String.length s in
    if slen >= 5 && String.sub s 0 5 = "false"
    then (false, String.sub s 5 (slen - 5))
    else if slen >= 4 && String.sub s 0 4 = "true"
    then (true, String.sub s 4 (slen - 4))
    else failwith "read_bool"

(* Unstaged scanf.  As with printf, the function is in CPS.
   Unlike printf the top-level continuation is part of the interface.
*)
let rec sscanfk : type a r. (a, r) fmt -> string -> a -> r * string =
  fun fmt s k -> match fmt with
    | Int ->        let i, s = read_int s in
                    (k i, s)
    | Lit l ->      let (), s = read_exact l s in
                    (k, s)
    | Bool ->       let i, s = read_bool s in
                    (k i, s)
    | Cat (l, r) -> let k1, s = sscanfk l s k in
                    let k2, s = sscanfk r s k1 in
                    (k2, s)

let sscanf : 'a 'b. ('a, 'b) fmt -> string -> 'a -> 'b =
  fun f s k -> fst (sscanfk f s k)

(* Naively-staged scanf.
*)
macro rec sscanf2k :
  type a r. (a, r) fmt -> string expr -> a expr -> (r * string) expr =
  fun fmt str k -> match fmt with
    | Int ->        << let i, s = read_int $str in
                       ($k i, s) >>
    | Lit l ->      << let _, s = read_exact $(Expr.of_string l) $str in
                       ($k, s) >>
    | Bool ->       << let i, s = read_bool $str in
                       ($k i, s) >>
    | Cat (l, r) -> << let k1, s = $(sscanf2k l str k) in
                       let k2, s = $(sscanf2k r <<s>> <<k1>>) in
                       (k2, s) >>
  
macro sscanf2 :
  type a r. (a, r) fmt -> (string -> a -> r) expr =
  fun fmt ->
    << fun s k ->
       fst $(sscanf2k fmt <<s>> <<k>>) >> 

(* Improved staged scanf using the pair-splitting let insertion.
   This generates significantly more compact code that avoids the
   nested let bindings and administrative pair splitting/building
   of the naively-staged version.
*)
macro rec sscanf3k :
  type a r. (a, r) fmt -> string expr -> a expr -> r  expr * string expr =
  fun fmt str k -> match fmt with
    | Int ->        let i, s = genlet2 <<read_int $str>> in
                    (<< $k $i >>, s)
    | Lit l ->      let _, s = genlet2 <<read_exact $(Expr.of_string l) $str>> in
                    (k, s)
    | Bool ->       let b, s = genlet2 <<read_bool $str>> in
                    (<< $k $b >>, s)
    | Cat (l, r) -> let k1, s = sscanf3k l str k in
                    let k2, s = sscanf3k r s k1 in
                    k2, s
  
macro sscanf3 :
  type a r. (a, r) fmt -> (string -> a -> r) expr =
  fun fmt ->
    << fun s k -> $(let_locus (fun () ->
                     let x, _ = sscanf3k fmt <<s>> <<k>> in x)) >> 
