(** © Jeremy Yallop, 2017-2018 *)

let () =
  begin
    assert
      (Fmt.sprintf [%fmt "(%d, %d)"] 2 3 = "(2, 3)");

    assert
      ($(Fmt.sprintf2 [%fmt "(%d, %d)"]) 2 3 = "(2, 3)");

    assert
      ($(Fmt.sprintf3 [%fmt "(%d, %d)"]) 2 3 = "(2, 3)");

    assert
      ($(Fmt.sprintf4 [%fmt "(%b, %b)"]) true false = "(true, false)");

    (* sscanf *)
    assert
      (Fmt.sscanf [%fmt "(%d, %d)"] "(2, 3)" (+) = 5);

    assert
      ($(Fmt.sscanf2 [%fmt "(%d, %d)"]) "(2, 3)" (+) = 5);

    assert
      ($(Fmt.sscanf3 [%fmt "(%d, %d)"]) "(2, 3)" (+) = 5);
  end

