(* Public interface of the combinator library *)

type 'a stream

(* Producers *)
macro of_arr : 'a array expr -> 'a stream
macro unfold : ('z expr -> ('a * 'z) option expr) -> 'z expr -> 'a stream

(* Consumer *)
macro fold : ('z expr -> 'a expr -> 'z expr) -> 'z expr -> 'a stream -> 'z expr
macro fold_tupled : ('z1 expr -> 'a expr -> 'z1 expr) -> 'z1 expr ->
                  ('z2 expr -> 'a expr -> 'z2 expr) -> 'z2 expr ->
                  'a stream -> ('z1 * 'z2) expr

(* Transformers *)
macro map      : ('a expr -> 'b expr) -> 'a stream -> 'b stream
macro filter   : ('a expr -> bool expr) -> 'a stream -> 'a stream
macro take     : int expr -> 'a stream -> 'a stream
macro flat_map : ('a expr -> 'b stream) -> 'a stream -> 'b stream
macro zip_with : ('a expr -> 'b expr -> 'c expr) ->
               ('a stream -> 'b stream -> 'c stream)
