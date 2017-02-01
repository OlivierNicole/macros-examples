# Staged streams

This is a direct copy of the amazing
[strymonas](https://github.com/strymonas/staged-streams.ocaml) library. All I
did was to replace MetaOCaml syntax with macro syntax, and lift a few modules.

```ocaml
$ ocamlc -dsource staged_streams_test.ml
open Stream_combinators_s
static (|>) = (^Pervasives).(|>) 
macro example () =
  ((of_arr (<< [|0;1;2;3|] >>)) |>
     (filter (fun x  -> << (($x) mod 2) = 0 >>)))
    |> (fold (fun z  -> fun a  -> << ($a) :: ($z) >>) (<< [] >>))
  
let () = List.iter (Printf.printf "%d\n") ($(example ())) 
splice #1:
let s_1 = Pervasives(*global*).ref []  in
           (let arr_2 = [|0;1;2;3|]  in
            for i_3 = 0 to
              Pervasives(*global*).(-) (Array(*global*).length arr_2) 1 do
              let el_4 = Array(*global*).get arr_2 i_3  in
              if
                Pervasives(*global*).(=) (Pervasives(*global*).(mod) el_4 2)
                  0
              then
                Pervasives(*global*).(:=) s_1 (el_4 ::
                  (Pervasives(*global*).(!) s_1))
            done);
           Pervasives(*global*).(!) s_1
```
