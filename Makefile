OCAMLBUILD=ocamlbuild -r -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

default: example_sieve

example:
	$(OCAMLBUILD) example.$(TARGET)

example_sieve:
	$(OCAMLBUILD) example_sieve.$(TARGET)

clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
