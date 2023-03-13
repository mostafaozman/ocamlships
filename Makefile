.PHONY: test check

zip: 
	rm -f battleship.zip
	zip -r battleship.zip . -x@exclude.lst

clean:
	dune clean
	rm -f battleship.zip

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

utop:
	OCAMLRUNPARAM=b dune utop src

doc:
	dune build @doc
	
opendoc: doc
	@bash opendoc.sh