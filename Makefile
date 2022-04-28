default: dune

dune:
	dune build

test:
	_build/install/default/bin/stark -l test/test.stark > test/test.llvm
	lli test/test.llvm

# %.native:
# 	ocamlbuild -use-ocamlfind $@
# 	mv $@ $*

clean :
	rm -rf test/*.llvm test/*.out _build/

.PHONY: dune test default clean