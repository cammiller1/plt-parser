# "ocamlbuild jpie.native" will also build the calculator

# "make test" Compiles everything and runs the regression tests
#.PHONY : test
#test : testall.sh
#	./testall.sh

.PHONY : all
all: parser.ml parser.mli scanner.ml 
#%.cmo : %.ml
#	ocamlc -c $<

#%.cmi : %.mli
#	ocamlc -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# Depedencies from ocamldep
#calc.cmo : scanner.cmo parser.cmi ast.cmi
#calc.cmx : scanner.cmx parser.cmx ast.cmi
#parser.cmo : ast.cmi parser.cmi
#parser.cmx : ast.cmi parser.cmi
#scanner.cmo : parser.cmi
#scanner.cmx : parser.cmx


##############################

#TARFILES = README Makefile scanner.mll ast.mli parser.mly calc.ml calc.tb

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml