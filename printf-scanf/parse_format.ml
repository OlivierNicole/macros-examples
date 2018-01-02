(** © Jeremy Yallop, 2017-2018 *)

(** Toy ppx parser for format strings.

    Turns [[%fmt s]] into a value of type Fmt.fmt.
    The available format specifiers are %d and %b.
*)

type fmt =
  | Int : fmt
  | Lit : string -> fmt
  | Bool : fmt
  | Cat : fmt * fmt -> fmt

let rec (%) x y = match x, y with
    Lit x, Lit y -> Lit (x ^ y)
  | Cat (x, Lit y), z -> Cat (x, Lit y % z)
  | x, Cat (Lit y, z) -> Cat (x, Lit y % z)
  | x, y -> Cat (x, y)

let rec lift : fmt -> Parsetree.expression =
  let id s = Asttypes.{ txt = Longident.parse s; loc = Location.none } in
  let open Ast_helper in function
    | Int -> Exp.construct (id "Fmt.Int") None
    | Lit s -> Exp.construct (id "Fmt.Lit")
                 (Some (Exp.constant (Const.string s)))
    | Bool -> Exp.construct (id "Fmt.Bool") None
    | Cat (x, y) -> Exp.construct (id "Fmt.Cat")
                      (Some (Exp.tuple [lift x; lift y]))

let failf fmt = Printf.kprintf failwith fmt

let parse (s : string) : fmt =
  let i = ref 0 in
  let len = String.length s in
  let fmt = ref (Lit "") in
  while !i < len do
    if s.[!i] = '%' then begin
      incr i;
      if !i = len then failf "format string %s ends with %%" s
      else if s.[!i] = 'b' then fmt := !fmt % Bool
      else if s.[!i] = 'd' then fmt := !fmt % Int
      else failf "Unknown format specifier %c" s.[!i]
    end
    else fmt := !fmt % Lit (String.make 1 s.[!i]);
    incr i
  done;
  !fmt

let fmt_mapper argv =
  let open Ast_mapper in
  let open Asttypes in
  let open Parsetree in
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc =
          Pexp_extension ({ txt = "fmt"; loc }, pstr)} ->
        begin match pstr with
        | PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Pconst_string (fmt, None))}, _)}] ->
          lift (parse fmt)
        | _ ->
          raise (Location.Error (
                  Location.error ~loc "[%fmt] accepts a string, e.g. [%fmt \"(%d, %d)\"]"))
        end
      | x -> default_mapper.expr mapper x }

let () = Ast_mapper.register "fmt" fmt_mapper
