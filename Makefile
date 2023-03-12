.PHONY: test check

zip: 
	rm -f adventure.zip
	zip -r adventure.zip . -x@exclude.lst

clean:
	dune clean
	rm -f adventure.zip

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

utop:
	OCAMLRUNPARAM=b dune utop src

doc:
	dune build @doc