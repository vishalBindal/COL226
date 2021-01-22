all: tokenize.mll goallex.mll parser.mly goalparser.mly interpreter.ml stage.ml
	ocamllex tokenize.mll     
	ocamllex goallex.mll 
	ocamllex nextlex.mll
	ocamlyacc parser.mly 
	ocamlyacc goalparser.mly
	ocamlyacc nextparser.mly    
	ocamlc -c interpreter.ml
	ocamlc -c parser.mli
	ocamlc -c goalparser.mli
	ocamlc -c nextparser.mli
	ocamlc -c tokenize.ml
	ocamlc -c goallex.ml
	ocamlc -c nextlex.ml
	ocamlc -c parser.ml
	ocamlc -c goalparser.ml
	ocamlc -c nextparser.ml
	ocamlc -c stage.ml
	ocamlc -o toy interpreter.cmo tokenize.cmo goallex.cmo nextlex.cmo parser.cmo goalparser.cmo nextparser.cmo stage.cmo

run:
	$(MAKE)
	./toy data.pl

clean:
	rm *.mli tokenize.ml goallex.ml nextlex.ml parser.ml goalparser.ml nextparser.ml *.cmo *.cmi toy
