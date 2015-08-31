/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

/*
  Port specific closure code. The garbage collector also contains
  some port specific closure code, might want to move it into this
  file someday. Better yet, make port specific versions of this file.
  */

#include "lisp.h"
#include "closure.h"
#include "limits.h"

void flush_cache(char *p);

LP OE = 0;				/* outer env is passed through this */
 
void flush_cache_addresses(void *start, int byte_count) {
  int i;
  char *ptr = (char *) start;
  for (i = 0; i < byte_count; i++) {
    /* Could use fewer flushes, since flush operates on double words... */
    flush_cache(ptr + i);
  }
}

LP new_closure(LP code, LP env) {
  CLOSURE *trampoline;
  LP procedure;
  unsigned int code_offset;

  trampoline = (CLOSURE *)
               (alloc_bytes(sizeof(CLOSURE),TYPE_CLOSURE) - 1);
  // printf("alloc trampoline %x with env %x\n", trampoline, env);

#ifdef linux_pc
#if (__WORDSIZE == 64)
  /*  Put OE addr into $r1, then low and high env addr into OE.
      Next put code ptr into $rax and jmp to it.
  */
  trampoline->movq1 = 0xbb49;
  (*(unsigned long *)(trampoline->oe_addr)) = (unsigned long) &OE;
  trampoline->movl1[0] = 0x41;  
  trampoline->movl1[1] = 0xc7;
  trampoline->movl1[2] = 0x03;
  (*(unsigned int *)(trampoline->env_low32)) = PTR_TO_INT(env);
  (*(unsigned int *)(trampoline->movl2)) = 0x0443c741;
  (*(unsigned int *)(trampoline->env_high32)) = ((unsigned int) \
						 (((unsigned long) env) \
						  >> 32));

  (*(unsigned short *)trampoline->movq2) = 0xbb49;
  (*(unsigned long *)(trampoline->code_ptr)) = (unsigned long) code;
  trampoline->jmp[0] = 0x41;
  trampoline->jmp[1] = 0xff;
  trampoline->jmp[2] = 0xe3;
  trampoline->nop = 0x90;
#else

  trampoline->movl = 0x05c7;
  CLOSURE_OE_ADDR(trampoline) = &OE;
  CLOSURE_ENV(trampoline) = env;
  trampoline->movleax = 0xb8;
  CLOSURE_CODE(trampoline) = code;
  trampoline->jmpeax0 = 0xff;
  trampoline->jmpeax1 = 0xe0;
#endif
#endif


#ifdef M68K
  trampoline->movl = 0x23fc;
  trampoline->env = env;
  trampoline->oe_addr = (LP) &OE;
  trampoline->jmpl = 0x4ef9;  
  trampoline->code_ptr = code;
#endif

#ifdef MIPS
  trampoline->lui_t0 = 0x3c08;
  trampoline->oe_hi = HI_16(env);
  trampoline->lui_t1 = 0x3c09;
  trampoline->code_ptr_hi = HI_16(code);
  trampoline->lui_t2 = 0x3c0a;
  trampoline->oe_addr_hi = HI_16(&OE);
  trampoline->ori_t2 = 0x354a;
  trampoline->oe_addr_low = LOW_16(&OE);
  trampoline->ori_t1 = 0x3529;
  trampoline->code_ptr_low = LOW_16(code);
  trampoline->ori_t0 = 0x3508;
  trampoline->oe_low = LOW_16(env);
  trampoline->jr_t1 = 0x01200008;
  trampoline->sw_t0_t2 = 0xad480000;
#endif 

#ifdef SPARC
 trampoline->save = 0x9de3bf88;
 trampoline->sethi_oe_opcode = 0x84;
 trampoline->sethi_oe = HI22(env);
 trampoline->add_oe_opcode = 0x50021;
 trampoline->add_oe = LOW10(env);
 trampoline->sethi_oe_addr_opcode = 0x8c;
 trampoline->sethi_oe_addr = HI22(&OE);
 trampoline->sethi_code_opcode = 0x94; 
 trampoline->sethi_code = HI22(code);
 trampoline->st_oe_opcode = 0x70123;
 trampoline->st_oe = LOW10(&OE);
 trampoline->jmpl_opcode = 0x40E25;
 trampoline->jmpl_code = LOW10(code);
 trampoline->restore = 0x81e80000;
  /* Synch i+d caches if they're separate. */
 flush_cache_addresses(trampoline, sizeof(CLOSURE));
#endif 

  procedure = NEW_PROCEDURE;
  LHEADER(procedure) =  (CLOSED_PROCEDURE_FLAG << 8) + TAG(procedure);
  DEREF(procedure) = (unsigned long) trampoline;
  return(procedure);
}

LP closure_oe(LP procedure) {
  CLOSURE *trampoline;
  LP oe;

  trampoline = (CLOSURE *) (LDREF(procedure,PROCEDURE,code_pointer));
#ifdef x86
#if (__WORDSIZE == 64)
  
#else
  oe = (LP) CLOSURE_ENV(trampoline);
#endif
#endif

#ifdef SPARC
  oe = (LP) ((trampoline->sethi_oe << 10) + trampoline->add_oe);
#endif
  return(oe);
}

/* LEN must be > 0 since we need space for a forwarding pointer */
LP new_oe(long len) {
  LP ptr;
  ptr = alloc_words(len,TYPE_OE);
  return(ptr);
}


