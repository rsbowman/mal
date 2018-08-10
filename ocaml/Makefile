SRC = util.ml mal.ml reader.ml env.ml eval.ml main.ml

PKGS = base,stdio,ppx_sexp_conv,ppx_compare,str,unix

all: main

main: $(SRC)
	ocamlfind ocamlopt -w A-4-44 -package $(PKGS) -linkpkg -o $@ $(SRC)

clean:
	rm -f main *.cmo *.cmi *.cmx *.o
