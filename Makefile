# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable

.PHONY : all
all : compyled.native concat.o

# "make compyled.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

compyled.native :
	opam config exec -- \
	rm -f *.o
	ocamlbuild -use-ocamlfind compyled.native
	gcc -c concat.c
	clang -emit-llvm -o concat.bc -c concat.c -Wno-varargs

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm
	rm -rf _build
	rm -rf testall.log *.diff *.ll


# Testing the "concat" example

printbig : concat.c
	cc -o concat -DBUILD_TEST concat.c