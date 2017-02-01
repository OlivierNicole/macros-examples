open Stream_combinators_s

static (|>) = ~Pervasives.(|>)
macro example () =
    of_arr << [| 0;1;2;3 |] >>
      |> filter (fun x   -> << $x mod 2 = 0 >>)
      |> fold   (fun z a -> << $a :: $z >>) <<[]>>

let () =
  List.iter (Printf.printf "%d\n")
    $(example ())
