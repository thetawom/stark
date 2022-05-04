default: dune

dune:
	dune build

test: dune
	_build/install/default/bin/stark -l test/test.stark > test/test.ll
	lli test/test.ll

# %.native:
# 	ocamlbuild -use-ocamlfind $@
# 	mv $@ $*

clean :
	rm -rf test/*.ll test/*.out _build/

.PHONY: dune test default clean