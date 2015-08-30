/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

LP p_lsp_SELECT_2DRESTART_2DOPTION(int argc);
LP p_lsp_ABORT_2DTO_2DTOP_2DLEVEL(int argc);

void select_restart_option() {
  p_lsp_SELECT_2DRESTART_2DOPTION(0);
}

void abort_to_top_level() {
  p_lsp_ABORT_2DTO_2DTOP_2DLEVEL(0);
}

void lisp_break() {
  int pid = getpid();

  kill(pid,SIGINT);
}

void lisp_debug() {
  lisp_break();
  abort_to_top_level();
}

void arg_limit_exceeded(int count) {
  printf("Call arg limit exceeded. %d args were passed, but only %d are allowed\n",count,CALL_ARG_LIMIT);
  lisp_debug();
}

void wna(ARGC actual, ARGC desired) {
  if (MV_HOLDER_P(actual) && (((MV *) actual)->argc == desired)) {
    return;
  } else {
    printf("Wrong number of arguments: %d args were received, but %d were expected\n",
	   REAL_ARGC(actual),desired);
    lisp_debug();
  }
}

void wna_low(ARGC actual, ARGC min) {
    printf("Wrong number of arguments: only %d args were received, but at least %d were expected\n",
	   REAL_ARGC(actual),min);
    lisp_debug();
}


void wna_high(ARGC actual, ARGC max) {
    printf("Wrong number of arguments: %d args were received, but at most %d were expected\n",
	   REAL_ARGC(actual),max);
    lisp_debug();
}
