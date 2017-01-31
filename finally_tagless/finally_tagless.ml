module type Symantics = sig
  type ('c, 'sv, 'dv) repr
  macro int : int  -> ('c, int, int) repr
  macro bool: bool -> ('c, bool, bool) repr
  macro lam :
    (('c, 'sa, 'da) repr -> ('c, 'sb, 'db) repr as 'x) ->
      ('c, 'x, 'da -> 'db) repr
  macro app :
    ('c, 'x, 'da -> 'db) repr ->
      (('c, 'sa, 'da) repr -> ('c, 'sb, 'db) repr as 'x)
  macro fix :
    ('x -> 'x) -> (('c, ('c, 'sa, 'da) repr ->
      ('c, 'sb, 'db) repr, 'da -> 'db) repr as 'x)
  macro add :
    ('c, int, int) repr -> ('c, int, int) repr -> ('c, int, int) repr
  macro mul :
    ('c, int, int) repr -> ('c, int, int) repr -> ('c, int, int) repr
  macro leq :
    ('c, int, int) repr -> ('c, int, int) repr -> ('c, bool, bool) repr
  macro if_ : ('c, bool, bool) repr
    -> (unit -> 'x) -> (unit -> 'x) -> (('c, 'sa, 'da) repr as 'x)
end

module EX(S : Symantics) = struct
  open S
  macro test1 () = app (lam (fun x -> x)) (bool true)
  macro testpowfix () =
    lam (fun x -> fix (fun self -> lam (fun n ->
      if_ (leq n (int 0)) (fun () -> int 1)
        (fun () -> mul x (app self (add n (int (-1))))))))
  macro testpowfix7 () = lam (fun x -> app (app (testpowfix ()) x) (int 7))
end

module S_sta : Symantics
  with type ('c, 'sv, 'dv) repr = 'dv
= struct
  open ~Pervasives
  type ('c, 'sv, 'dv) repr = 'dv
  macro int x = x
  macro bool b = b
  macro lam f = fun x -> f x
  macro app f x = f x
  macro fix f = let rec self n = f self n in self
  macro add x y = x + y
  macro mul x y = x * y
  macro leq x y = x <= y
  macro if_ eb et ee = if eb then et () else ee ()
end

module R = EX(S_sta)

let () = Printf.printf "%d\n" @@
  $(Expr.of_int (R.testpowfix7 () 9))

module S_dyn : Symantics
  with type ('c, 'sv, 'dv) repr = 'dv expr
= struct
  type ('c, 'sv, 'dv) repr = 'dv expr
  macro int (x : int) = Expr.of_int x
  macro bool (b : bool) = Expr.of_bool b
  macro lam f = << fun x -> $(f <<x>>) >>
  macro app f x = << $f $x >>
  macro fix f = << let rec self n = $(f <<self>>) n in self >>
  macro add x y = << $x + $y >>
  macro mul x y  = << $x * $y >>
  macro leq x y = << $x <= $y >>
  macro if_ eb et ee = << if $eb then $(et ()) else $(ee ()) >>
end

module C = EX(S_dyn)

let () = Printf.printf "%d\n" @@
  $(C.testpowfix7 ()) 9

module P : sig
  include Symantics
  static val abstr : ('c, 'sv, 'dv) repr -> 'dv expr
end = struct
  type ('c, 'sv, 'dv) repr =
    {st: 'sv option; dy: 'dv expr}
  static abstr {dy = x} = x
  static pdyn x = {st = None; dy = x}

  macro int (x : int) =
    {st = Some (S_sta.int x); dy = S_dyn.int x}
  macro bool (x : bool) =
    {st = Some (S_sta.bool x); dy = S_dyn.bool x}
  macro add e e' = match (e, e') with
  | {st = Some 0}, e | e, {st = Some 0} -> e
  | {st = Some m}, {st = Some n} -> int (S_sta.add m n)
  | _ -> pdyn (S_dyn.add (abstr e) (abstr e'))
  macro mul e e' = match (e, e') with
  | {st = Some 1}, e | e, {st = Some 1} -> e
  | {st = Some 0}, _ | _, {st = Some 0} -> int (S_sta.int 0)
  | {st = Some m}, {st = Some n} -> int (S_sta.mul m n)
  | _ -> pdyn (S_dyn.mul (abstr e) (abstr e'))
  macro if_ eb et ee = match eb with
  | {st = Some b} -> if b then et () else ee ()
  | _ ->
      pdyn (S_dyn.if_ (abstr eb) (fun () -> abstr (et ()))
        (fun () -> abstr (ee ())))
  macro leq x y = match (x, y) with
  | {st = Some x}, {st = Some y} -> bool (S_sta.leq x y)
  | _ -> pdyn (S_dyn.leq (abstr x) (abstr y))

  macro lam f =
    {st = Some f; dy = S_dyn.lam (fun x -> abstr (f (pdyn x)))}
  macro app ef ea = match ef with
  | {st = Some f} -> f ea
  | _ -> pdyn (S_dyn.app (abstr ef) (abstr ea))
  macro fix f =
    let fdyn = S_dyn.fix (fun x -> abstr (f (pdyn x))) in
    let rec self =
      function
      | {st = Some _} as e -> app (f (lam self)) e
      | e -> pdyn (S_dyn.app fdyn (abstr e))
    in
    {st = Some self; dy = fdyn}
end

module Part = EX(P)

let () = Printf.printf "%d\n" @@
  $(P.abstr (Part.testpowfix7 ())) 9
