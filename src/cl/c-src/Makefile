#  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. 

# HEY! try to compile cleanly without -w
# make clean; make linux-pc-64
# HEY! Put -O2 back in 
WCC = gcc -w -xc -pipe -g -fPIC -c

CC = gcc -xc -pipe -g -fPIC -c

linux-pc: x86 linux-pc-socket.o linux-pc-cache-flush.o linux-interrupts.o

linux-pc-64: x86-64 linux-pc-socket.o linux-pc-cache-flush.o linux-interrupts.o

x86-64:	common x86-64-only

x86:	common x86-only

x86-only: x86-integer-arith.o x86-apply.o

x86-64-only: x86-64-integer-arith.o x86-64-apply.o

common:	 chars.o arith.o  array.o generic-loader.o  \
	 memory.o io.o closure.o init.o time.o error.o funcall.o \
	 dynamic.o misc.o eval-code.o

dynamic.o: lisp.h
	$(CC) -c    dynamic.c

time.o:	lisp.h
	$(CC) -c    time.c

init.o:	lisp.h
	$(CC) -c    init.c

error.o: lisp.h
	$(CC) -c    error.c

funcall.o: lisp.h
	$(CC) -c    funcall.c

misc.o:	lisp.h
	$(CC) -c    misc.c

# Eval always needs debugging info if we want to debug
# interpreted code. GCC with -O and -g seems to screw up the debugging
# info, so we need to compile with -g only.
eval-code.o:	lisp.h eval-code.c
	$(CC) -c -g  eval-code.c

memory.o:	lisp.h memory.c
	$(WCC) -c    memory.c

chars.o:	lisp.h chars.c
	$(CC) -c    chars.c

arith.o:	lisp.h arith.c
	$(CC) -c    arith.c

array.o:	lisp.h array.c
	$(CC) -c    array.c	

io.o:		lisp.h io.c
	$(CC) -c    io.c

closure.o:	lisp.h closure.c
	$(CC) -c    closure.c

linux-pc-socket.o:	lisp.h 
	$(CC) -c    -DUNIXCONN -o socket.o linux-pc-socket.c

linux-interrupts.o:	lisp.h
	$(CC) -c    -o interrupts.o linux-interrupts.c

generic-loader.o:	lisp.h
	$(CC) -c    generic-loader.c

x86-integer-arith.o: x86-integer-arith.s
	$(CC) -fPIC -c -o integer-arith.o x86-integer-arith.s

x86-64-integer-arith.o: x86-64-integer-arith.s
	$(AS) -o integer-arith.o x86-64-integer-arith.s

generic-apply.o: generic-apply.c
	$(CC) -c  -o apply.o generic-apply.c

x86-apply.o: x86-apply.s
	$(AS) -o apply.o x86-apply.s

x86-64-apply.o: x86-64-apply.s
	$(AS) --gdwarf2 -o apply.o x86-64-apply.s

linux-pc-cache-flush.o:	linux-pc-cache-flush.s
	$(AS) -o cache-flush.o linux-pc-cache-flush.s

clean: 
	rm -f *.o


