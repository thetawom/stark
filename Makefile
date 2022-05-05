default: dune

dune:
	dune build

run: dune
	_build/install/default/bin/stark -l test/test.stark > test/test.ll
	lli test/test.ll

test: dune
	_build/install/default/bin/stark -l test/test.stark > test/test.ll
	lli test/test.ll > test/test0.out
	diff test/test0.out test/correct0.out -w

# %.native:
# 	ocamlbuild -use-ocamlfind $@
# 	mv $@ $*

clean :
	rm -rf test/*.llvm test/test*.out _build/

.PHONY: dune run test default clean