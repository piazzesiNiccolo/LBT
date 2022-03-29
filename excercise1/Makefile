TARGET=bin/main

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native: 
	ocamlbuild -use-ocamlfind $@
	

clean:
	ocamlbuild -clean

.PHONY: clean default