/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <stdio.h>

LP p_lsp_COERCE_2DTO_2DFUNCTION (ARGC argc, LP procedure);
LP ubf_handler (ARGC argc, ...);
LP apply_function_1 (ARGC argc, LP (*func) (ARGC dummy, ...),
				  LP a[],int num_args);

LP apply_function(ARGC argc, LP procedure, int spread_last_p, va_list ap) {
  LP a[APPLY_ARGS_LIMIT];
  LP rest;
  int count;
  int numargs;
  LP (*func) (ARGC dummy, ...);

  count = 0;
  numargs = REAL_ARGC(argc) - 1 - spread_last_p;

  if (numargs > APPLY_ARGS_LIMIT) {
    printf("APPLY_ARGS_LIMIT exceeded\n");
    lisp_debug();
  }

  while (count < numargs) {
    a[count] = NEXT_VAR_ARG;
    count = count + 1;
  }

  if (spread_last_p == 1) {
    rest = NEXT_VAR_ARG;
    if (LISTP(rest)) {
      while (rest != NIL) {
	a[count] = LDREF(rest,CONS,car);
	count = count + 1;
	if (count > APPLY_ARGS_LIMIT) {
	  printf("APPLY_ARGS_LIMIT exceeded\n");
	  lisp_debug();
	} else {
	  rest = LDREF(rest,CONS,cdr);
	}
      }
    } else {
      printf("Last argument to APPLY is not a list\n");
      lisp_debug();
    }
  }
  argc = MV_CALL(argc,count);
  func = CODE_PTR(procedure);
  return(apply_function_1(argc,func,a,count));
}

LP ubf_handler(ARGC argc, ...) {
  int i;
  LP args[CALL_ARG_LIMIT];

  BEGIN_ANSI_VAR_ARGS(argc);
  argc = REAL_ARGC(argc);
  for (i = 0; i < argc; i++) {
    args[i] = NEXT_VAR_ARG;
  }
  END_VAR_ARGS;

  printf("Undefined function handler called with %ld %s\n",
	 argc,((argc == 1) ? "argument" : "arguments"));

  printf("Undefined function handler\n");
  lisp_debug();
}

LP p_lsp_FUNCALL(ARGC argc, LP procedure, ...) {
  LP result;

  BEGIN_ANSI_VAR_ARGS(procedure);
  procedure = p_lsp_COERCE_2DTO_2DFUNCTION(1,procedure);
  result = apply_function(argc,procedure,0,ap);
  END_VAR_ARGS;
  return(result);
}

LP p_lsp_APPLY(ARGC argc, LP procedure, ...) {
  LP result;

  BEGIN_ANSI_VAR_ARGS(procedure);
  procedure = p_lsp_COERCE_2DTO_2DFUNCTION(1,procedure);
  result = apply_function(argc,procedure,1,ap);
  END_VAR_ARGS;
  return(result);
}

/* Exactly like apply, but hidden from the debugger */
LP p_lsp_EVAL_2DAPPLY(ARGC argc, LP procedure, ...) {
  LP result;

  BEGIN_ANSI_VAR_ARGS(procedure);
  procedure = p_lsp_COERCE_2DTO_2DFUNCTION(1,procedure);
  result = apply_function(argc,procedure,1,ap);
  END_VAR_ARGS;
  return(result);
}

