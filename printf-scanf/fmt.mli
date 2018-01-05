(** © Jeremy Yallop, 2017-2018 *)

(** {1 Typed format strings for staged printing and parsing} *)

(** Typed format strings  *)
type (_, _) fmt =
    Int : (int -> 'a, 'a) fmt
  | Lit : string -> ('a, 'a) fmt
  | Bool : (bool -> 'a, 'a) fmt
  | Cat : ('a, 'b) fmt * ('b, 'c) fmt -> ('a, 'c) fmt

(** {2 Printing}  *)

(** Unstaged printing with typed format strings *)
val sprintf : ('a, string) fmt -> 'a

(** A naively staged version of {!sprintf} *)
macro sprintf2 : ('a, string) fmt -> 'a expr

(** An improved staged version of {!sprintf} based on partially-static
    strings *)
macro sprintf3 : ('a, string) fmt -> 'a expr

(** A further improved staged version of {!sprintf} using delimited
    control for code motion *)
macro sprintf4 : ('a, string) fmt -> 'a expr

(** {2 Parsing}  *)

(** Unstaged parsing with typed format strings *)
val sscanf : ('a, 'b) fmt -> string -> 'a -> 'b

(** Naively-staged parsing with typed format strings *)
macro sscanf2 : ('a, 'r) fmt -> (string -> 'a -> 'r) expr

(** An improved staged version of {!sscanf_staged} using delimited
    control for code motion *)
macro sscanf3 : ('a, 'r) fmt -> (string -> 'a -> 'r) expr
