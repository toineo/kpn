OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

default: example_crible

example:
	$(OCAMLBUILD) example.$(TARGET)

example_crible:
	$(OCAMLBUILD) example_crible.$(TARGET)

clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
