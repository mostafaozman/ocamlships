.PHONY: test check

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

utop:
	OCAMLRUNPARAM=b dune utop src

doc:
	dune build @doc