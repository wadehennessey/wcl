/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <time.h>

static int start_realtime;

/* Unix time exceeded a fixum sometime in 2004, so we return unix time
   less 1,000,000,000 */
int unix_time_of_day () {
  struct timeval tp;
  struct timezone tzp;

  gettimeofday(&tp,&tzp);
  return(tp.tv_sec - 1000000000);
}

void init_run_time() {
  // Clock should already be running
  // I don't think this does anything, since it ignores teh return value
  clock();			/*  start the runtime clock */
}

int timeval_to_internal_time(struct timeval *tp) {
  return(tp->tv_sec * 100 + tp->tv_usec / 10000);
}

int internal_system_run_time() {
  struct rusage stats;

  getrusage(RUSAGE_SELF,&stats);
  return(timeval_to_internal_time(&(stats.ru_stime)));
}

int internal_user_run_time() {
  struct rusage stats;

  getrusage(RUSAGE_SELF,&stats);
  return(timeval_to_internal_time(&(stats.ru_stime)));
}

int internal_real_time () {
  struct timeval tp;
  struct timezone tzp;

  gettimeofday(&tp,&tzp);
  tp.tv_sec = tp.tv_sec - start_realtime;
  return(timeval_to_internal_time(&tp));
}

void init_real_time() {
  struct timeval tp;
  struct timezone tzp;

  gettimeofday(&tp,&tzp);
  start_realtime = tp.tv_sec;
}

int unix_timezone() {
  extern time_t timezone;
  tzset();
  return(timezone / 3600);
}

int unix_daylight_savings_time() {
  extern int daylight;

  tzset();
  return(daylight);
}


