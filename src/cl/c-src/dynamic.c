/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <stdio.h>
#include <setjmp.h>

UW_POINT *uw_top = NULL;

UW_POINT *find_uw_tag(LP tag, int type) {
  UW_POINT *uwp;

  for (uwp = uw_top; uwp != NULL; uwp = uwp->next) {
    if ((uwp->type == type) &&
	(uwp->name == (LP) tag)) /* why is a cast needed? */
      break;
  }
  if (uwp == NULL) {
    switch (type) {
    
    case UW_CATCH:
      printf("No CATCH tag found\n");
      lisp_debug();

    case UW_DYNAMIC_TAG:
      printf("Cannot GO to a closed over tag which no longer exists!\n");
      lisp_debug();

    otherwise: printf("internal unwind error\n");
    }
  } else {
    return(uwp);
  }
}

void verify_chain() {
  UW_POINT *uwp;

  for (uwp = uw_top; uwp != NULL; uwp = uwp->next) {
    printf("address = %x, type = %d\n",uwp,uwp->type);
  }
}

int unwind(UW_POINT *dest, LP value) {
  while (uw_top != NULL) {
    if  ((UW_POINT *) dest == uw_top) {	/* Why is the cast needed? */
      uw_top->single_throw_value = value;
      // use single_throw_value
      longjmp(uw_top->c_env, 2); /* done */
    } else {
      switch (uw_top->type) {

      case UW_CATCH:
      case UW_DYNAMIC_TAG:
	POP_UW_POINT;		/*  skip unwanted catch and tag frames */
	break;

      case UW_SPECBIND:
	LDREF(uw_top->name,SYMBOL,value) = uw_top->value;
	POP_UW_POINT;
	break;

      case UW_PROTECT:
	/* The protect form uses the following info to continue
	   the unwind */
	uw_top->continue_dest = (UW_POINT *)dest; /* why is a cast needed? */
	uw_top->value = (LP) value; /* why is a cast needed? */
	longjmp(uw_top->c_env,-1); 
	break;
      }
    }
  }
  printf("ERROR: Unwind exhausted the uw_chain\n");
}

LP throw(LP tag, LP value, MV *mv_holder) {
  UW_POINT *dest;
  int i;

  dest = find_uw_tag(tag,UW_CATCH);
  /* Pass multiple values if we have them and
     the catcher wants them */
  if ((MV_HOLDER_P(dest->mv_holder)) && (mv_holder->return_flag == 1)) {
    dest->mv_holder->return_flag = 1;
    dest->mv_holder->argc = mv_holder->argc;
    /* Copy values into dest holder. Look, no Consing! */
    for (i = 0; i < mv_holder->argc; i++) {
      dest->mv_holder->values[i] = mv_holder->values[i];
    }
  }
  unwind(dest,value);
}


LP dynamic_go(LP tag) {
  UW_POINT *dest;

  dest = find_uw_tag(tag,UW_DYNAMIC_TAG);
  unwind(dest, (LP) 1);
}
