/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"

LP apply_function_1(ARGC argc,
		    LP (*func) (ARGC dummy, ....),
		    LP a[],
		    int num_args) {
  LP r;

  switch (num_args) {
    
  case 0: r = (*func)(argc); break;
  case 1: r = (*func)(argc,a[0]); break;
  case 2: r = (*func)(argc,a[0],a[1]); break;
  case 3: r = (*func)(argc,a[0],a[1],a[2]); break;
  case 4: r = (*func)(argc,a[0],a[1],a[2],a[3]); break;
  case 5: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4]); break;
  case 6: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5]); break;
  case 7: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6]); break;
  case 8: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]); break;
  case 9: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		      a[8]); break;
  case 10: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],
		       a[9]); break;
  case 11: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10]); break;
  case 12: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11]); break;
  case 13: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12]); break;
  case 14: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13]); break;
  case 15: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14]); break;
  case 16: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15]); break;
  case 17: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16]); break;
  case 18: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17]); break;
  case 19: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18]); break;
  case 20: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19]); break;
  case 21: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20]); break;
  case 22: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21]); break;
  case 23: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22]); break;
  case 24: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23]); break;
  case 25: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23],
		       a[24]); break;
  case 26: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23],
		       a[24],a[25]); break;
  case 27: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23],
		       a[24],a[25],a[26]); break;
  case 28: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23],
		       a[24],a[25],a[26],a[27]); break;
  case 29: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23],
		       a[24],a[25],a[26],a[27],a[28]); break;
  case 30: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23],
		       a[24],a[25],a[26],a[27],a[28],a[29]); break;
  case 31: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23],
		       a[24],a[25],a[26],a[27],a[28],a[29],a[30]); break;
  case 32: r = (*func)(argc,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],
		       a[8],a[9],a[10],a[11],a[12],a[13],a[14],a[15],
		       a[16],a[17],a[18],a[19],a[20],a[21],a[22],a[23],
		       a[24],a[25],a[26],a[27],a[28],a[29],a[30],
		       a[31]); break;
  default: printf("Internal generic APPLY error\n"); lisp_debug(); break;
  }

  return(r);
}
