default: dune

dune:
	dune build

run: dune
	_build/install/default/bin/stark -l test/test.stark > test/test.ll
	lli test/test.ll

test: dune
	_build/install/default/bin/stark -l test/test0.stark > test/test.ll
	lli test/test.ll
	lli test/test.ll > test/test0.out
	diff test/test0.out test/correct0.out -w
	_build/install/default/bin/stark -l test/test1.stark > test/test1.ll
	lli test/test1.ll
	lli test/test1.ll > test/test1.out
	diff test/test1.out test/correct1.out -w
	_build/install/default/bin/stark -l test/test2.stark > test/test2.ll
	lli test/test2.ll
	lli test/test2.ll > test/test2.out
	diff test/test2.out test/correct2.out -w
	_build/install/default/bin/stark -l test/helloworld.stark > test/helloworld.ll
	lli test/helloworld.ll
	_build/install/default/bin/stark -l test/gcd.stark > test/gcd.ll
	lli test/gcd.ll

# %.native:
# 	ocamlbuild -use-ocamlfind $@
# 	mv $@ $*

clean :
	rm -rf test/*.llvm test/test*.out _build/

.PHONY: dune run test default clean