# "make test" Compiles everything and runs the regression tests

#.PHONY : test
#test : all testall.sh
#	./testall.sh

# "make all" builds the executable

.PHONY : all
all : jpie.native

# "make jpie.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

jpie.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind jpie.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf _build