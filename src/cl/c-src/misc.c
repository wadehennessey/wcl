/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <sys/utsname.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

LP ubf_handler(ARGC argc, ...);
LP p_lsp_REPL(int argc);
void init_library_cl();

void start_initialization(int argc, char *argv[],
			  int dynamic_memory_size, int static_memory_size);

PROCEDURE ubf_procedure = {TYPE_PROCEDURE, (LP) ubf_handler};
UBV ubv_marker= {TYPE_UBV, 0};

/*
runtime for VALUES (written in Lisp for now)
LP p_lsp_VALUES(ARGC argc, ...)
{
  if (MV_HOLDER_P(argc)) {
    get each arg and put it into the mv struct;
  } else {
    if (argc == 0) {
      return(NIL);
    } else {
      return(first_var_arg);
    }
  }
}
*/

/* Could have written this one in Lisp more easily. */
LP lookup_keyword(LP kwd, LP l) {
  LP rest;
  LP next;

  rest = l;
  while (rest != NIL) {
    next = LDREF(rest,CONS,cdr);
    if (LDREF(rest,CONS,car) == kwd) {
      if (next == NIL) {
	printf("Missing keyword argument\n");
	lisp_debug();
      } else {
	return(LDREF(next,CONS,car));
      }
    } else {
      rest = LDREF(next,CONS,cdr);
    }
  }
  return((LP) UBK_MARKER);
}

LP initialize_symbol(LP symbol, LP name, LP hashcode) {
  SYMBOL *sym;
  sym = (SYMBOL *) (symbol - (sizeof(long) + 1));
  sym->self_link = symbol;
  sym->name = name;
  sym->value = UBV_MARKER;
  sym->package = NIL;
  sym->plist = NIL;
  sym->function = LREF(ubf_procedure);
  sym->hashcode = hashcode;
  sym->flags = 0;
  return(symbol);
}
  
LP make_symbol(LP name, LP hashcode) {
  LP symbol = alloc_words((sizeof(SYMBOL)/sizeof(long) - 1),TYPE_SYMBOL);
  return(initialize_symbol(symbol, name, hashcode));
}

LP make_static_symbol(LP name, LP hashcode) {
  // HEY! use static_alloc_words
  LP symbol = alloc_words((sizeof(SYMBOL)/sizeof(long) - 1),TYPE_SYMBOL);
  return(initialize_symbol(symbol, name, hashcode));
}

LP new_foreign_ptr(LP type, LP ptr) {
  LP  result;
  FOREIGN_POINTER *fptr;

  if (ptr == 0) {
    return(0);			/* Make C Null pointer = Lisp 0 */
  } else {
    result = alloc_words((sizeof(FOREIGN_POINTER)/sizeof(long) - 1),
			 TYPE_FOREIGN_PTR);
    fptr = (FOREIGN_POINTER *) (result - (sizeof(long) + 1));
    fptr->pointer = ptr;
    fptr->type = type;
    return(result);
  }
}

int getosversion(char *name, int namelen)
{
  struct utsname sysinfo;

  uname(&sysinfo);
  strcpy(name,&(sysinfo.release[0]));
  return(1);
}

int host_bits_per_word() {
  return(sizeof(long) * 8);
}

void repl() {
  p_lsp_REPL(0);
}

void init_wcl(int dynamic_size, int static_size) {
  char *arg1 = "embedded_wcl";
  char **argv = &arg1;

  start_initialization(1,argv,dynamic_size, static_size);
  init_library_cl();
}
