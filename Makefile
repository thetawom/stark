all : stark.native

stark.native : stark.ml parser.mly scanner.mll ast.ml
	ocamlbuild stark.native

stark.out : stark.native
	./stark.native < test.stark > test.out

testing: stark.native
	./stark.native < test1.stark > test1.out
	diff correctTest1.out test1.out


# ##############################


.PHONY : all test clean rebuild

test : stark.native stark.out testing

clean :
	rm -rf *.native *.out _build/

rebuild : clean all
