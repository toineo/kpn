OCAMLBUILD=ocamlbuild -r -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

default: example_sieve

example:
	$(OCAMLBUILD) example.$(TARGET)

example_sieve:
	$(OCAMLBUILD) example_sieve.$(TARGET)

telegraph:
	$(OCAMLBUILD) Telegraph/server.$(TARGET) Telegraph/receiver.$(TARGET) Telegraph/sender.$(TARGET)

netwatch:
	$(OCAMLBUILD) NetWatch/server.$(TARGET) NetWatch/receiver.$(TARGET) NetWatch/sender.$(TARGET)

clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
