# DO NOT EDIT THIS FILE

.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f dna.zip
	zip -r dna.zip . -x@exclude.lst

clean:
	dune clean
	rm -f dna.zip