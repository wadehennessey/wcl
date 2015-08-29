/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"

extern LP p_lsp_EVAL_2DSEQUENCE(ARGC argc, LP v_FORMS_0, LP v_VENV_1, LP v_FENV_2, LP v_TENV_3, LP v_BENV_4);
extern LP p_lsp_EVAL_2DHAIRY_2DEXTEND_2DVAR_2DENV(ARGC argc, LP v_VENV_0, LP v_LAMBDA_2DLIST_1, LP v_ARGS_2, LP v_FENV_3, LP v_TENV_4, LP v_BENV_5);
extern LP p_lsp_EVAL_2DUNDO_2DSPECIAL_2DBINDINGS(ARGC argc, LP v_SPECIALS_0, LP v_SPECIAL_2DVALUES_1);
extern LP p_lsp_EVAL_2DSIMPLE_2DEXTEND_2DVAR_2DENV(ARGC argc, LP v_CURRENT_2DENV_0, LP v_NEW_2DVARS_1, LP v_NEW_2DVALS_2);

char* gdb_lambda_string = "(LAMBDA ...)";

LP use_arg(LP x) {
  return(x);
}

/* All of the evaluator could be (and once was) written entirely in Lisp.
   However, in order to make debugging evaluated code work well, 
   these evalator functions are written in C so that GDB can 
   recognize and manipulate them.
*/
LP eval_closure_code(ARGC argc, ...) {
  LP env;
  LP interpreted_function_name;
  LP formal_args; LP evaled_args; LP body;
  LP venv; LP fenv; LP tenv; LP benv;
  LP result;
  DYNAMIC_REST_HOLDER(rest_conses);
  int real_argc;

  BEGIN_ANSI_VAR_ARGS(argc);
  real_argc = REAL_ARGC(argc);
  env = OE;
  DYNAMIC_RESTIFY(evaled_args,1,NEXT_VAR_ARG);
  END_VAR_ARGS;
  interpreted_function_name = GET_OE_SLOT(env,0);
  formal_args = GET_OE_SLOT(env,1);
  body = GET_OE_SLOT(env,2);
  venv = GET_OE_SLOT(env,3);
  fenv = GET_OE_SLOT(env,4);
  tenv = GET_OE_SLOT(env,5);
  benv = GET_OE_SLOT(env,6);
  if (LISTP(formal_args)) {
    venv = (LP) p_lsp_EVAL_2DSIMPLE_2DEXTEND_2DVAR_2DENV
      (3, venv, formal_args, evaled_args);
    result = (LP) p_lsp_EVAL_2DSEQUENCE (MV_CALL(argc,5),
					 body, venv, fenv, tenv, benv);
  } else {
    /* We could do all this in Lisp, but then the correct VENV
       (including special bindings which aren't really in VENV)
       wouldn't be readily available to the debugger; hence this mess. */
    LP specials;
    LP special_values;
    BEGIN_UW_PROTECT_BODY
      BEGIN_MV_CALL(mv_holder,0);
      venv = (LP) p_lsp_EVAL_2DHAIRY_2DEXTEND_2DVAR_2DENV
	(MV_CALL(mv_holder,6),
	 venv, formal_args, evaled_args, fenv, tenv, benv);
    BEGIN_VAR_VALUES;
    NEXT_VAR_VALUE(mv_holder);	/* skip uninitialized first value */
    specials = NEXT_VAR_VALUE(mv_holder);
    special_values = NEXT_VAR_VALUE(mv_holder);
    END_VAR_VALUES;
    END_MV_CALL;
    result = (LP) p_lsp_EVAL_2DSEQUENCE (MV_CALL(argc,5),
					 body, venv, fenv, tenv, benv);
    BEGIN_UW_PROTECT_CLEANUP
      p_lsp_EVAL_2DUNDO_2DSPECIAL_2DBINDINGS(2,specials,special_values);
    CONTINUE_FROM_PROTECT;
  }
  /* UG! Don't let optimizer discard name.
     Shouldn't volatile work here??? It doesn't. */
  use_arg(interpreted_function_name);
  return(result);
}

LP make_eval_closure(LP name, LP formal_args, LP body,
		     LP venv, LP fenv, LP tenv, LP benv) {
  LP closure; LP oe;

  oe = NEW_OE(7);
  SET_OE_SLOT(oe,0,name);
  SET_OE_SLOT(oe,1,formal_args);
  SET_OE_SLOT(oe,2,body);
  SET_OE_SLOT(oe,3,venv);
  SET_OE_SLOT(oe,4,fenv);
  SET_OE_SLOT(oe,5,tenv);
  SET_OE_SLOT(oe,6,benv);
  closure = MAKE_CLOSURE(eval_closure_code,oe);
  return(closure);
}

