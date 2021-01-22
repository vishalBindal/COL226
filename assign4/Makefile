all: tokenize.mll parser.mly sheet.ml
	ocamllex tokenize.mll      
	ocamlyacc parser.mly     
	ocamlc -c parser.mli
	ocamlc -c tokenize.ml
	ocamlc -c functions.ml
	ocamlc -c parser.ml
	ocamlc -c sheet.ml
	ocamlc -o assignment4 str.cma functions.cmo tokenize.cmo parser.cmo sheet.cmo

clean:
	rm *.cm* parser.ml parser.mli tokenize.ml assignment4
