.PHONY: test

zip: 
	rm -f ships.zip
	zip -r ships.zip . -x@exclude.lst

clean:
	dune clean
	rm -f ships.zip

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

utop:
	OCAMLRUNPARAM=b dune utop src

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc
	
opendoc: doc
	@bash opendoc.sh