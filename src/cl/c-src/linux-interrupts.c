/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <signal.h>

extern void lisp_debug();

signal_handler (int sig, int code, struct sigcontext *scp) {
  printf("WCL: Ignoring signal number %d, aborting to top-level\n",sig);
  abort_to_top_level();
}

