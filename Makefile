all: calc

calc: ast.cmo lexer.cmo parser.cmo checker.cmo normalization.cmo calc.cmo
	ocamlc -o $@ $?

%.cmo: %.ml
	ocamlc -c $<

parser.cmi: parser.mli
	ocamlc -c $<

parser.ml parser.mli: parser.mly
	ocamlyacc $<

lexer.ml: lexer.mll parser.cmi
	ocamllex lexer.mll

clean:
	rm -f *.cmo *.cmi lexer.ml parser.mli parser.ml calc

testchecker: ast.cmo checker.cmo
	ocaml testchecker.ml
