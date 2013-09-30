export OCAMLPATH:=$(OCAMLPATH):$(PWD)/../lib

CAMLP4=
OCAMLC_OPTS=-package eterm,oUnit
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=
NON_LIB_MODULES=of_bytes.ml

BYTE_TARGETS=$(NON_LIB_MODULES:%.ml=%.byte)
NATIVE_TARGETS=$(NON_LIB_MODULES:%.ml=%.native)

.PHONY: all install test

all: byte-code native-code

of_bytes.native: of_bytes.cmx

of_bytes.byte: of_bytes.cmo

test: byte-code native-code
	./of_bytes.native
	./of_bytes.byte

# Dependencies
include ../lib/Ocamlrules.mk.in
