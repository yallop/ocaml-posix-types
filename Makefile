.PHONY: build test install uninstall reinstall clean

FINDLIB_NAME=posix-types
MOD_NAME=posix_types

CTYPES_LIB_DIR=$(shell ocamlfind query ctypes)

OCAMLBUILD=CTYPES_LIB_DIR=$(CTYPES_LIB_DIR) \
	ocamlbuild -use-ocamlfind -classic-display

TARGETS=.cma .cmxa

PRODUCTS=$(addprefix $(MOD_NAME),$(TARGETS))
PRODUCTS += lib$(MOD_NAME)_stubs.a dll$(MOD_NAME)_stubs.so

TYPES=.mli .cmi .cmti

INSTALL:=$(addprefix $(MOD_NAME),$(TYPES)) \
         $(addprefix $(MOD_NAME),$(TARGETS))

INSTALL:=$(addprefix _build/lib/,$(INSTALL))

ARCHIVES:=_build/lib/$(MOD_NAME).a

build:
	$(OCAMLBUILD) $(PRODUCTS)

install:
	ocamlfind install $(FINDLIB_NAME) META \
		$(INSTALL) \
		-dll _build/lib/dll$(MOD_NAME)_stubs.so \
		-nodll _build/lib/lib$(MOD_NAME)_stubs.a \
		$(ARCHIVES)

uninstall:
	ocamlfind remove $(FINDLIB_NAME)

reinstall: uninstall install

clean:
	ocamlbuild -clean
	rm -f lib/$(MOD_NAME).cm? lib/$(MOD_NAME).o
