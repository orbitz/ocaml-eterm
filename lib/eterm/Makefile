CAMLP4=
OCAMLC_OPTS=-package num,bitstring,bitstring.syntax -syntax camlp4o
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=eterm.ml
NON_LIB_MODULES=eterm_lexer.ml eterm_parser.ml

BYTE_TARGETS=eterm.cma
NATIVE_TARGETS=eterm.cmxa

.PHONY: all install test

all: native-code byte-code

install: all
	ocamlfind install eterm META eterm.cmxa eterm.cma eterm.a *.cmx *.cmi *.mli

eterm_lexer.ml: eterm_lexer.mll
	ocamllex $^

eterm_parser.ml: eterm_parser.mly
	ocamlyacc $^

test:

clean: parser_clean

parser_clean:
	rm eterm_parser.ml eterm_lexer.ml eterm_parser.mli

# Dependencies
include ../Ocamlrules.mk.in
