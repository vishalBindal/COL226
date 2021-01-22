all: tokenize.mll parser.mly sheet.ml
	ocamllex tokenize.mll      
	ocamlyacc parser.mly     
	ocamlc -c parser.mli
	ocamlc -c tokenize.ml
	ocamlc -c parser.ml
	ocamlc -c sheet.ml
	ocamlc -o sheet tokenize.cmo parser.cmo sheet.cmo

run: 
	$(MAKE)
	./sheet