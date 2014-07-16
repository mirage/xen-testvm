# Generated by Mirage (Fri, 23 May 2014 12:37:05 GMT).

LIBS   = -pkgs lwt.syntax,mirage-block-xen.front,mirage-console-xen,mirage-types.lwt,testvmlib,vchan
PKGS   = mirage-console-xen mirage-xen
SYNTAX = -tags "syntax(camlp4o),annot,bin_annot,strict_sequence,principal"

FLAGS  = -cflag -g -lflags -g,-linkpkg,-dontlink,unix

BUILD  = ocamlbuild -classic-display -use-ocamlfind $(LIBS) $(SYNTAX) $(FLAGS)
OPAM   = opam

XEN_LIBDIR := $(shell ocamlfind query mirage-xen)

.PHONY: all prepare clean build main.native
all: build

prepare:
	$(OPAM) install $(PKGS)

main.native:
	$(BUILD) main.native

main.native.o:
	$(BUILD) main.native.o

build: main.native.o
	ld -d -nostdlib -m elf_x86_64 -T $(XEN_LIBDIR)/mirage-x86_64.lds $(XEN_LIBDIR)/x86_64.o \
	  _build/main.native.o $(XEN_LIBDIR)/libocaml.a $(XEN_LIBDIR)/libxen.a \
	  $(XEN_LIBDIR)/libxencaml.a $(XEN_LIBDIR)/libdiet.a $(XEN_LIBDIR)/libm.a \
	  $(XEN_LIBDIR)/longjmp.o -o mir-test.xen

run: build
	@echo test.xl has been created. Edit it to add VIFs or VBDs
	@echo Then do something similar to: xl create -c test.xl
clean:
	ocamlbuild -clean
