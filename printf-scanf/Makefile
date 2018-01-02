all: fmtlib.cma fmt_ppx.byte

fmtlib.cma: fmt.mli fmt.ml
	ocamlfind c -a -o fmtlib.cma -package delimcc -m delimcc.cma fmt.mli fmt.ml

fmt_ppx.byte: parse_format.ml
	ocamlfind c -o fmt_ppx.byte -package delimcc,compiler-libs.common -linkpkg \
            -m delimcc.cma \
            parse_format.ml

demo: fmtlib.cma fmt_ppx.byte
	ocamlfind c -o demo.byte -dsource -ppx ./fmt_ppx.byte -package delimcc -m delimcc.cma demo.ml 
	./demo.byte

clean:
	rm -f *.cmo *.cmi *.cmm a.out *.byte *.cma

.PHONY: all clean all
