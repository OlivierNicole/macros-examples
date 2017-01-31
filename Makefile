.PHONY: all clean test

PROJECTS=staged_streams_test finally_tagless

TARGETS=$(foreach project,$(PROJECTS),$(project).native)

all:
	ocamlbuild $(TARGETS)

clean:
	ocamlbuild -clean

test:
	@ for target in $(TARGETS); do ./$(TARGET); done
