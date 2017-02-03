open Ocamlbuild_plugin;;

dispatch begin
  function
  | After_rules ->
    dep ["ocaml"; "compile"; "byte"; "stream_combinators_dep"]
      ["staged-streams/stream_combinators_s.cmo"];
    dep ["ocaml"; "compile"; "native"; "stream_combinators_dep"]
      ["staged-streams/stream_combinators_s.cmx"];
  | _ -> ()
end;;
