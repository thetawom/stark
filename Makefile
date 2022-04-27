all : stark.native

stark.native : stark.ml parser.mly scanner.mll ast.ml
	ocamlbuild -pkgs llvm stark.native

test.llvm : stark.native
	./stark.native -l test.stark > test.llvm

test.out: test.llvm
	lli test.llvm > test.out


# ##############################


.PHONY : all test clean rebuild

test : test.out

clean :
	rm -rf *.native *.out *.llvm _build/

rebuild : clean all
