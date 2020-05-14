CC=gcc
CFLAGS= -g -Wall
LDFLAGS = -g

default: oops.native libfoo.a

oops.native:
	ocamlbuild -use-ocamlfind oops.native -pkgs llvm

libfoo.a: foo.o
	ar -crs libfoo.a foo.o
	ranlib libfoo.a

foo.o: foo.h foo.c

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *.native
	rm -f *.o *.a *.s a.out *.byte