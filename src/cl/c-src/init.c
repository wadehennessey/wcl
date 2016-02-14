/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#if RTGC
#include "/home/wade/rtgc/allocate.h"
#endif

extern char *optarg;
typedef void (*sighandler_t)(int);
void signal_handler(int signal);
void init_memory_allocator(int dynamic_memory_size, int static_memory_size);
void init_run_time();
void init_real_time();
void init_arith();
sighandler_t signal(int signum, sighandler_t handler);
LP p_lsp_START_2DAPPLICATION(ARGC argc, LP v_MAIN_2DFUNCTION_0);


#define DEFAULT_DYNAMIC_MEMORY_SIZE 8192
#define DEFAULT_STATIC_MEMORY_SIZE 512

int command_line_argc;
char **command_line_argv;

int get_memory_size(int argc, char *argv[],
		    char *optstr, char opt, int default_size) {
  int c,size;

  /* HEY! Should change arg processing to use a while loop */
  c = getopt(argc,argv,optstr);
  if (c == opt) {
    sscanf(optarg,"%d",&size);
    return(size);
  } else {
    return(default_size);
  }
}

void unbuffer_interactive_streams() {
  /*  fcntl(1,F_SETFL,O_NDELAY);
      fcntl(2,F_SETFL,O_NDELAY);
      */
  /*  setbuf(stdout,NULL);
      setbuf(stderr,NULL); */
}

void init_signals() {
  signal(SIGFPE, signal_handler);
  signal(SIGTRAP, signal_handler);
  signal(SIGBUS, signal_handler);
  // signal(SIGSYS, signal_handler);
  signal(SIGSEGV, signal_handler);
  signal(SIGILL, signal_handler);
  signal(SIGINT, signal_handler);
}

LP command_line_argument(int n) {
  if ((n >= 0) && (n <= command_line_argc)) {
    return((LP) c_to_lisp_string(command_line_argv[n]));
  } else {
    return(NIL);
  }
}

void start_initialization(int argc, char *argv[],
		     int dynamic_memory_size, int static_memory_size) {
  LP result;

  command_line_argc = argc;
  command_line_argv = argv;
  /* Override args with cmd line switches for now.
     Pass -1 or something to indicate use switch settings? */
  dynamic_memory_size =
    get_memory_size(argc,argv,"m:",'m',DEFAULT_DYNAMIC_MEMORY_SIZE);
  static_memory_size = 
    get_memory_size(argc,argv,"s:",'s',DEFAULT_STATIC_MEMORY_SIZE);
#if RTGC
  RTinit_heap(1L << 30, 0);
#else
  init_memory_allocator(dynamic_memory_size,static_memory_size);
#endif
  init_run_time();
  init_real_time();
  init_arith();
  init_signals();
  unbuffer_interactive_streams();
}

static
void *start_main_thread(void *start_func) {
  p_lsp_START_2DAPPLICATION(1,start_func);
}

void init_wcl_threads(LP start_func) {
#if RTGC
  new_thread(&start_main_thread, (void *) start_func);
  while (1);
#else
  p_lsp_START_2DAPPLICATION(1,start_func);
#endif
}
