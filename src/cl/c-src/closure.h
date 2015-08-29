/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

/* All closures should end on a 32-bit word boundry 

   HEY! Need to worry about I/D cache splits, write-through vs. copy back
   and IBUF problems on some machines. So far, SPARC seems to be an easy one.
*/

#include <limits.h>


#ifdef x86

#if (__WORDSIZE == 64)
/* x86_64 code */
/* Argh! Structure padding makes it impossible to do this more simply */
typedef struct closure {
  unsigned short movq1;
  unsigned char oe_addr[8];
  unsigned char movl1[3];
  unsigned char env_low32[4];
  unsigned char movl2[4];
  unsigned char env_high32[4];
  unsigned char movq2[2];
  unsigned char code_ptr[8];
  unsigned char jmp[3];
  unsigned char nop;
} CLOSURE;

#define CLOSURE_ENV_LOW32(t) (*((unsigned int *) (((CLOSURE *)t)->env_low32)))
#define CLOSURE_ENV_HIGH32(t) (*((unsigned int *) \
				 (((CLOSURE *)t)->env_high32)))
#define CLOSURE_ENV(t) ((CLOSURE_ENV_HIGH32(t) << 32) || CLOSURE_ENV_LOW32(t)

#else
/* x86_32 code */
/* Argh! Structure padding makes it impossible to do this more simply */
typedef struct closure {
  unsigned short movl; // 0x05c7
  unsigned short oe_low;
  unsigned short oe_high;
  unsigned short env_low;
  unsigned short env_high;
  unsigned char  movleax; // 0xb8
  unsigned char code_offset0;  // dest - &padding
  unsigned char code_offset1;
  unsigned char code_offset2;
  unsigned char code_offset3;
  unsigned char jmpeax0;  // 0x0e
  unsigned char jmpeax1;  // 0xff
  unsigned char pad0;
  unsigned char pad1;
  unsigned char pad2;
} CLOSURE;

/* It's faster to just just blast the bits directly rather than picking
   them apart into chars and shorts */
#define CLOSURE_OE_ADDR(t) *((unsigned int *) (((char *) t) + 2))
#define CLOSURE_ENV(t) *((unsigned int *) (((char *) t) + 6))
#define CLOSURE_CODE(t) *((unsigned int *) (((char *) t) + 11))

#endif

#endif

/* HEY! I think we can  elimintate the save/restore by using %g2 -> %g4
   as temp regs. SPARC ABI says:

   %g0 is always zero
   %g1 is a caller-save `very temp'
   %g2..%g4 are reserved for application code and may not
            be modified by libraries
   %g5..%g7 may be read but not written by an application
           (the analog of %g2..%g4 for libraries)
*/

#ifdef SPARC
#define HI22(x) ((((unsigned long) x) & 0xfffffc00) >> 10)
#define LOW10(x) (((unsigned long) x) & 0x3ff)

typedef struct sparc_closure {
  unsigned long save;

  unsigned long sethi_oe_opcode:10;
  unsigned long sethi_oe:22;

  unsigned long add_oe_opcode:19;
  unsigned  long add_oe:13;

  unsigned long sethi_oe_addr_opcode:10; 
  unsigned long sethi_oe_addr:22;

  unsigned long sethi_code_opcode:10;
  unsigned  long sethi_code:22;

  unsigned long st_oe_opcode:19;
  unsigned long st_oe:13;

  unsigned long jmpl_opcode:19;
  unsigned long jmpl_code:13;

  unsigned long restore;
} CLOSURE;
#endif
