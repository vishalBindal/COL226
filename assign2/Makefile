all:
	ocamllex tokenize.mll
	ocamlopt -o tokenize tokenize.ml

run: 
	$(MAKE)
	./tokenize
