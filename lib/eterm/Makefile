CAMLP4=
OCAMLC_OPTS=-package num,bitstring,bitstring.syntax -syntax camlp4o
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=eterm.ml
NON_LIB_MODULES=

BYTE_TARGETS=eterm.cma
NATIVE_TARGETS=eterm.cmxa

.PHONY: all install test

all: native-code byte-code

install: all
	ocamlfind install eterm META eterm.cmxa eterm.cma eterm.a *.cmx *.cmi *.mli


test:

# Dependencies
include ../Ocamlrules.mk.in
