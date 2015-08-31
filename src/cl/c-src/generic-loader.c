/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"

LP p_lsp_INTERN(int argc, LP name, LP package);
LP p_lsp_MAKE_2DSYMBOL(int argc, LP name);

#define DIGIT_TO_INT(digit) \
  ((digit <= '9') ? (digit - '0') : 10 + (digit - 'A'))

#define UPCASE(char) (((char >= 'a') || (char <= 'z')) ? \
                     (char - ('a' - 'A')) : char)

/* Call when loader encounters an undefined symbol */
LP intern_if_needed(char *c_name) {
  char *tmp,*ptr;
  char package_name[100],symbol_name[100];
  LP lisp_symbol_name;
  LP lisp_package_name;
  LP sym;
  int i;

  ptr = c_name;
  if ((*ptr == 's') && (*(ptr + 1) == '_')) {
    ptr = ptr + 2;		/*  skip leader */
    i = 0;
    while (*ptr != '_') {
      package_name[i] = UPCASE(*ptr);
      ptr= ptr + 1;
      i = i + 1;
    }
    package_name[i] = NULL;
    ptr = ptr + 1;
    tmp = symbol_name;
    while (*ptr != NULL) {
      if (*ptr == '_') {
	*tmp = (DIGIT_TO_INT(*(ptr + 1)) << 4) + DIGIT_TO_INT(*(ptr + 2));
	ptr = ptr + 3;
      } else {
	*tmp = *ptr++;
      }
      tmp = tmp + 1;
    }
    *tmp = NULL;
    lisp_symbol_name = copy_c_to_lisp_string(symbol_name);
    if (*package_name == NULL) {
      /* uninterned symbol */
      sym = p_lsp_MAKE_2DSYMBOL(1,lisp_symbol_name);
    } else {
      lisp_package_name = copy_c_to_lisp_string(package_name);
      sym = p_lsp_INTERN(2,lisp_symbol_name,lisp_package_name);
    }
    return(sym - 5);		/* account for LREF in c code */
  } else {
    return(0);
  }
}
