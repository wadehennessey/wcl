/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <stdlib.h>
#include <sys/mman.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include "closure.h"
#include "limits.h"
#include "mcheck.h"

#include "/home/wade/rtgc/allocate.h"

#ifdef x86
#define NUMBER_OF_REGS_TO_SCAN  8
#endif

#ifdef x86_64
#define NUMBER_OF_REGS_TO_SCAN  16
#endif

#define PAGE_POWER 9

#define BYTES_PER_PAGE (1 << PAGE_POWER)
#define WORDS_PER_PAGE (BYTES_PER_PAGE / sizeof(LP))
#define PAGE_OFFSET_MASK (BYTES_PER_PAGE - 1)
#define PAGE_BYTE_OFFSET(ptr) ((long) ptr & PAGE_OFFSET_MASK)
#define PAGE_TO_ADDRESS(page) ((LP) (first_ptr + (page << PAGE_POWER)))
#define ADDRESS_TO_PAGE(ptr) ((ptr - first_ptr) >> PAGE_POWER)
#define NEXT_PAGE(page) ((page == last_page) ? 0 : (page + 1))
#define ROUND_TO_WORD(x) ((((long) x % sizeof(LP)) == 0) ? x : \
                         x + (sizeof(long) - ((long) x & (sizeof(long) - 1))))
#define ROUND_TO_PAGE(x) ((((long) x % BYTES_PER_PAGE) == 0) ? x : \
                          x + (BYTES_PER_PAGE - ((long) x & PAGE_OFFSET_MASK)))

/* All objects must have at least enough bytes of storage to hold 
   a forwarding pointer, even if the length field contains 0. */
#define CORRECT_ZERO_OBJECT_SIZE(x) ((x == 0) ? sizeof(LP) : x)

#define IN_HEAP(ptr) ((((LP) ptr) >= first_ptr) && \
                      (((LP) ptr) <= frontier_limit_ptr))


#define DOUBLE_ALIGNMENT_MASK (sizeof(double) - 1)
#define DOUBLE_ALIGNEDP(p) ((long) p == ((long)p & ~DOUBLE_ALIGNMENT_MASK))

#define END_LOCK_LIST -1
#define UNSCANNED     -2
#define CONTIG_LOCK   -3

#define PAGE_LOCKED(page) \
  ((pageinfo[page].generation == next_generation) && \
   (pageinfo[page].next_lock != UNSCANNED))


#define PAGE_UNLOCKED(page) \
  ((pageinfo[page].generation == next_generation) && \
   (pageinfo[page].next_lock == UNSCANNED))

LP least_positive_bignum;
LP least_negative_bignum;
//unsigned long w_lib_symbols[],w_app_symbols[];
//unsigned long key_lib_symbols[],key_app_symbols[];
LP p_lsp_FIND_2DOR_2DMAKE_2DPACKAGE();
LP p_lsp_ADD_2DSYMBOL(int argc, LP syms, LP package);
void copy_regs_to_stack(long *regs);

// HEY! why are these needed?
void full_gc();
void move_sub_objects(LP ptr);

typedef struct page_info {
  long next_lock;
  int generation;
  char contig_flag;
} PAGE_INFO;

typedef struct symbol_record {
  char *package_name;
  unsigned long *symbols;
  struct symbol_record *next;
} SYMBOL_RECORD;

/* Init these so the debugger can find them. Bletch... */

PAGE_INFO *pageinfo = 0;
LP first_ptr = 0;
LP frontier_ptr = 0;
LP frontier_limit_ptr = 0;

long current_generation = 0;
long next_generation = 0;
long last_page = 0;
long total_pages = 0;
long allocated_pages = 0;
long frontier_page = 0;
long lock_list = 0;

long remaining_page_bytes = 0;
unsigned long *stack_bottom = 0;
unsigned long *stack_top = 0;
long debug_gc = 0;

LP dynamic_frontier_ptr = 0;
LP dynamic_frontier_limit_ptr = 0;
long dynamic_remaining_page_bytes = 0;

long static_total_pages = 0;
LP static_first_ptr = 0;
LP static_frontier_ptr = 0;
LP static_frontier_limit_ptr = 0;
long static_remaining_page_bytes = 0;

//#define DEBUG_GC (debug_gc != 0)
#define DEBUG_GC 0

SYMBOL_RECORD *registered_symbols = 0;

long delay_symbol_interns = 1;

long inside_gc_flag = 0;
long print_gc_messages_flag = 0;
long gc_count = 0;

void switch_to_static_space() {
  dynamic_frontier_ptr = frontier_ptr;
  dynamic_frontier_limit_ptr = frontier_limit_ptr;
  dynamic_remaining_page_bytes = remaining_page_bytes;

  frontier_ptr = static_frontier_ptr;
  frontier_limit_ptr = static_frontier_limit_ptr;
  remaining_page_bytes = static_remaining_page_bytes;
}

void switch_to_dynamic_space() {
  static_frontier_ptr = frontier_ptr;
  static_frontier_limit_ptr = frontier_limit_ptr;
  static_remaining_page_bytes = remaining_page_bytes;

  frontier_ptr = dynamic_frontier_ptr;
  frontier_limit_ptr = dynamic_frontier_limit_ptr;
  remaining_page_bytes = dynamic_remaining_page_bytes;
}

long set_gc_messages(long n) {
  print_gc_messages_flag = n;
  return(n);
}

long heap_start() {
  return((long) first_ptr);
}

long heap_frontier() {
  return((long) frontier_ptr);
}

long heap_frontier_limit() {
  return((long) frontier_limit_ptr);
}

long heap_page_size() {
  return(BYTES_PER_PAGE);
}

long total_heap_pages() {
  return(total_pages);
}

long free_heap_pages() {
  long count,page,gen;

  count = 0;
  for (page = 0; page <= last_page; page = page + 1) {
    gen = pageinfo[page].generation;
    if ((gen != current_generation) && (gen != next_generation)) {
      count = count + 1;
    }
  }
  return(count);
}

long total_static_pages() {
  return(static_total_pages);
}

long free_static_bytes() {
  return(static_frontier_limit_ptr - static_frontier_ptr);
}

void terminate_page() {
  if (DEBUG_GC) {
    printf("terminate_page, frontier: %p, remaining: %d\n",
	   frontier_ptr,remaining_page_bytes);
  }
  if (remaining_page_bytes != 0) {
    *((LPL) frontier_ptr) = TYPE_END_OF_PAGE;
    remaining_page_bytes = 0;
  }
}

void allocate_pages(long n) {
  long limit_page,start_page,contig_count,i,generation;

  if (frontier_limit_ptr == static_frontier_limit_ptr) {
    printf("\nError: out of static space!\n");
    lisp_debug();
  }
  if ((n + allocated_pages) > (total_pages / 2)) {
    full_gc();
  }
  terminate_page();
  limit_page = frontier_page;
  contig_count = n;
  while (contig_count > 0) {
    generation = pageinfo[frontier_page].generation;
    if ((generation != current_generation) &&
	(generation != next_generation)) {
      if (contig_count == n) {
	start_page = frontier_page;
      }
      contig_count = contig_count - 1;
    } else {
      contig_count = n;
    }
    if (frontier_page == last_page) {
      frontier_page = 0;
      if (contig_count != 0) {
	contig_count = n;
      }
    } else {
      frontier_page = frontier_page + 1;
    }
    if (frontier_page == limit_page) {
      printf("Error: cannot find %d contiguous free pages. Must be out of memory\n",n);
      lisp_debug();
    }
  }
  if (DEBUG_GC) {
    printf("Alloc page, start: %d, len: %d, gen: %d\n",
	   start_page,n,next_generation);
  }
  allocated_pages = allocated_pages + n;
  frontier_ptr = PAGE_TO_ADDRESS(start_page);
  remaining_page_bytes = n * BYTES_PER_PAGE;
  memset(frontier_ptr, 0, remaining_page_bytes);
  pageinfo[start_page].generation = next_generation;
  pageinfo[start_page].contig_flag = 0;
  pageinfo[start_page].next_lock = UNSCANNED;
  n = n - 1;
  while (n > 0) {
    start_page = start_page + 1;
    pageinfo[start_page].generation  = next_generation;
    pageinfo[start_page].contig_flag = 1;
    pageinfo[start_page].next_lock = UNSCANNED;
    n = n - 1;
  }
}      

void *wcl_get_closure_env(LP ptr) {
  unsigned int oe_low32 = CLOSURE_ENV_LOW32(ptr);
  unsigned long oe_high32 = CLOSURE_ENV_HIGH32(ptr);
  unsigned long oe = (oe_high32 << 32) | oe_low32;
  return((void *) oe);
}

#if RTGC
LP wcl_wb(LPL lhs_address, LP rhs) {
  return(RTwrite_barrier(lhs_address, rhs));
}    

void *safe_malloc(size_t size, long tag) {
  void *ptr; 

  // Revert back to plain old RTpointers to avoid possible bugs in RTcustom1 
  //ptr = RTallocate(RTpointers, size);
  ptr = RTallocate(RTcustom1, size);
  
  if (0 == ptr) {
    printf("safe malloc failed!\n");
    lisp_debug();
  }
  return(ptr);
}

// All memory allocation goes through this function and c_cons.
//   However, these are not safe wrt to interrupts.
LP alloc_words_1(long num_words, long tag, long len_field) {
  long total_num_bytes;
  LP base_ptr;
  LP ptr;

  total_num_bytes = (num_words * sizeof(long)) + sizeof(long);
  base_ptr = safe_malloc(total_num_bytes, tag);
  ptr = base_ptr + sizeof(long) + 1;
  LHEADER(ptr) = (len_field << 8) + tag;
  return(ptr);
}

LP static_alloc_words(long num_words, long tag) {
  // HEY! need to write static_alloc_words_1 and call it
  LP ptr = alloc_words_1(num_words, tag, num_words); 
  return(ptr);
}

/* Save a few instructions consing. Not sure if this is worth
   duplicating the allocation code.  */
LP c_cons(LP x, LP y) {
  LP base_ptr;
  LP ptr;

  /* Install header BEFORE incrementing frontier so we don't have
     a pointer to bogus memory  */
  base_ptr = safe_malloc(sizeof(struct cons), TYPE_CONS);
  ptr = base_ptr + sizeof(long) + 1;
  LHEADER(ptr) = (2 << 8) + TYPE_CONS;
  LDREF(ptr,CONS,car) = x;
  LDREF(ptr,CONS,cdr) = y;
  return(ptr);
}

// Put these in rtgc/allocate.h? Call file rtgc.h?
//typedef unsigned long * LPTR;
typedef unsigned char * BPTR;
void RTscan_memory_segment(BPTR *low, BPTR *high);

static void trace_pointer(LP ptr) {
  // Only trace tagged pointers, not fixnums
  if (OTHER_PTRP(ptr)) {
    RTtrace_pointer(ptr);
  }
}

void scan_wcl_object(void *low, void *high) {
  // Hey! add check to be sure we dont trace fixnums
  LP ptr = (BPTR) low + sizeof(long) + 1;
  switch (TAG(ptr) & TAG_MASK) {
  case TYPE_CONS:
    trace_pointer(LDREF(ptr,CONS,car));
    trace_pointer(LDREF(ptr,CONS,cdr));
    break;

  case TYPE_PROCEDURE:
    switch (HEADER(ptr)) {
    case FUNCALLABLE_INSTANCE_HEADER:
      printf("funcallable_instace support is untested\n");
      lisp_debug();
      trace_pointer(ADD_TAG(LDREF(ptr,FUNCALLABLE_INSTANCE, code_pointer)));
      trace_pointer(LDREF(ptr,FUNCALLABLE_INSTANCE,wrapper));
      trace_pointer(LDREF(ptr,FUNCALLABLE_INSTANCE,slots));
      break;
    case CLOSED_PROCEDURE_HEADER:
      trace_pointer(ADD_TAG(LDREF(ptr,PROCEDURE,code_pointer)));
      break;
    }
    break;  

  case TYPE_CLOSURE:
    {
      BPTR env = wcl_get_closure_env((BPTR) low + sizeof(long));
      // We know that env isn't a fixnum and always points into the heap
      RTtrace_heap_pointer(env);
    }
    break;

  case TYPE_COMPLEX:
    trace_pointer(LDREF(ptr,COMPLEX,real));
    trace_pointer(LDREF(ptr,COMPLEX,imaginary));
    break;

  case TYPE_RATIO:
    trace_pointer(LDREF(ptr,RATIO,numerator));
    trace_pointer(LDREF(ptr,RATIO,denominator));
    break;
    
  case TYPE_SYMBOL:
    trace_pointer(LDREF(ptr,SYMBOL,value));
    trace_pointer(LDREF(ptr,SYMBOL,package));
    trace_pointer(LDREF(ptr,SYMBOL,name));
    trace_pointer(LDREF(ptr,SYMBOL,plist));
    trace_pointer(LDREF(ptr,SYMBOL,function));
    break;

  case TYPE_LINE_SYMBOL:
    printf("line_symbol support is untested!\n");
    lisp_debug();
    trace_pointer(LDREF(ptr,LINE_SYMBOL,self_link));
    break;

  case TYPE_OE:
  case TYPE_STRUCTURE:
  case TYPE_SIMPLE_VECTOR:
    {
      long i;
      long len = LEN_FIELD(ptr) * sizeof(long);
      for (i = 0; i < len; i = i + sizeof(long)) {
	trace_pointer((LP) DEREF(ptr + i));
      }
    }
    break;

  case TYPE_BIGNUM:
    break;

  case TYPE_CHARACTER:
  case TYPE_FLOAT:
  case TYPE_SIMPLE_BIT_VECTOR:
  case TYPE_SIMPLE_SIGNED_8BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_8BIT_VECTOR:
  case TYPE_SIMPLE_STRING:
  case TYPE_SIMPLE_SIGNED_16BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_16BIT_VECTOR:
  case TYPE_SIMPLE_SIGNED_32BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_32BIT_VECTOR:
  case TYPE_SIMPLE_FLOAT_VECTOR:
  case TYPE_PADDING:
    break;

  case TYPE_SIMPLE_BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_8BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_8BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_CHAR_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_16BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_16BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_32BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_32BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_PTR_MULTI_ARRAY:
  case TYPE_SIMPLE_FLOAT_MULTI_ARRAY:
      trace_pointer(LDREF(ptr,SIMPLE_MULTI_ARRAY,underlying_vector));
      trace_pointer(LDREF(ptr,SIMPLE_MULTI_ARRAY,dims_vector));
      trace_pointer(LDREF(ptr,SIMPLE_MULTI_ARRAY,multiplier_vector));
      break;

    
  case TYPE_COMPLEX_BIT_VECTOR:
  case TYPE_COMPLEX_SIGNED_8BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_8BIT_VECTOR:
  case TYPE_COMPLEX_CHAR_VECTOR:
  case TYPE_COMPLEX_SIGNED_16BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_16BIT_VECTOR:
  case TYPE_COMPLEX_SIGNED_32BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_32BIT_VECTOR:
  case TYPE_COMPLEX_PTR_VECTOR:
  case TYPE_COMPLEX_FLOAT_VECTOR:
    trace_pointer(LDREF(ptr,COMPLEX_VECTOR,underlying_vector));
    /* Ignore fill pointer and displaced-index offset */
    break;

  case TYPE_COMPLEX_BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_8BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_8BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_CHAR_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_16BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_16BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_32BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_32BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_PTR_MULTI_ARRAY:
  case TYPE_COMPLEX_FLOAT_MULTI_ARRAY:
    trace_pointer(LDREF(ptr,COMPLEX_MULTI_ARRAY,underlying_vector));
    trace_pointer(LDREF(ptr,COMPLEX_MULTI_ARRAY,dims_vector));
    trace_pointer(LDREF(ptr,COMPLEX_MULTI_ARRAY,multiplier_vector));
    /* Ignore displaced-index offset */
    break;

  case TYPE_FOREIGN_PTR:
    break;

  case TYPE_VOID:
  default:
    printf("scan_wcl_object error: unknown tag: %x\n",TAG(ptr));
    lisp_debug();
  }
}

void scan_wcl_static_symbols() {
  SYMBOL_RECORD *ptr;
  unsigned long *syms;

  for (ptr = registered_symbols; ptr != 0; ptr = ptr->next) {
    for (syms = ptr->symbols; *syms != 0; syms = syms + 1) {
      BPTR *low = (BPTR *) ((*syms) - 1);
      BPTR *high =  (BPTR *) ((BPTR) low + (sizeof(struct symbol) - 
					    sizeof(unsigned long)));
      RTscan_memory_segment(low, high);
    }
  }
}
#else
LP wcl_wb(LPL lhs_address, LP rhs) {
  return((LP) (*lhs_address = (LD) rhs));
}    

LP alloc_words_1(long num_words, long tag, long len_field) {
  long total_num_bytes;
  LP ptr;

  total_num_bytes = (num_words * sizeof(long)) + sizeof(long);
  if (total_num_bytes > remaining_page_bytes) {
    if (total_num_bytes <= BYTES_PER_PAGE) {
      allocate_pages(1);
    } else {
      allocate_pages((total_num_bytes + BYTES_PER_PAGE - 1) / BYTES_PER_PAGE);
    }
  }
  /* Install header BEFORE incrementing frontier so we don't have
     a pointer to bogus memory  */
  ptr = frontier_ptr + sizeof(long) + 1;
  LHEADER(ptr) = (len_field << 8) + tag;
  frontier_ptr =  frontier_ptr + total_num_bytes;
  remaining_page_bytes = remaining_page_bytes - total_num_bytes;
  return(ptr);
}

LP static_alloc_words(long num_words, long tag) {
  switch_to_static_space();
  LP ptr = alloc_words_1(num_words, tag, num_words); 
  switch_to_dynamic_space();
  return(ptr);
}
  
/* Save a few instructions consing. Not sure if this is worth
   duplicating the allocation code.  */
LP c_cons(LP x, LP y) {
  LP ptr;

  if (sizeof(struct cons) > remaining_page_bytes) {
    allocate_pages(1);
  }
  /* Install header BEFORE incrementing frontier so we don't have
     a pointer to bogus memory  */
  ptr = frontier_ptr + sizeof(long) + 1;
  LHEADER(ptr) = (2 << 8) + TYPE_CONS;
  LDREF(ptr,CONS,car) = x;
  LDREF(ptr,CONS,cdr) = y;
  frontier_ptr =  frontier_ptr + sizeof(struct cons);
  remaining_page_bytes = remaining_page_bytes - sizeof(struct cons);
  return(ptr);
}
#endif

LP alloc_words(long num_words, long tag) {
  return(alloc_words_1(num_words, tag, num_words));
}

LP alloc_memory(long num_units, long unit_size, long tag) {
  LP ptr;
  long num_bits,num_words;

#if (__WORDSIZE == 64)
  if ((tag & ARRAY_ELEMENT_TYPE_MASK) == ELEMENT_TYPE_PTR) {
    if (unit_size != 64) {
      unit_size = 64;
    }
  }
#endif
  num_bits = num_units * unit_size;
  num_words = (num_bits / (sizeof(long) * 8)) +
              (((num_bits & ((sizeof(long) * 8) - 1)) == 0) ? 0 : 1);
  return(alloc_words_1(num_words,tag,num_units));
}

LP alloc_bits(long num_bits, long tag) {
  long num_words;
  num_words = (num_bits / (sizeof(long) * 8)) +
              (((num_bits & ((sizeof(long) * 8) - 1)) == 0) ? 0 : 1);
  return(alloc_words_1(num_words,tag,num_bits));
}


LP alloc_bytes(long num_bytes, long tag) {
  long num_words;
  num_words = (num_bytes / sizeof(long)) +
              (((num_bytes & (sizeof(long) - 1)) == 0) ? 0 : 1);
  return(alloc_words_1(num_words,tag,num_bytes));
}

LP alloc_shorts(long num_shorts, long tag) {
  long num_words;
  num_words = (num_shorts / (sizeof(long) / sizeof(short))) +
              (((num_shorts & ((sizeof(long) / sizeof(short)) - 1)) == 0) ?
	       0 : 1);
  return(alloc_words_1(num_words,tag,num_shorts));
}

/* floats Must be 8 byte aligned! */
LP alloc_float() {
  LP ptr;
  long float_size;

  float_size = sizeof(struct double_float);
  /* No problem if frontier is already double aligned OR 
     we'll have to start at the beginning of a new page */
  if (((sizeof(long) == 8) ||
       DOUBLE_ALIGNEDP(frontier_ptr)) || 
      remaining_page_bytes < float_size) { 
    /* Account for header size when allocating float */
    ptr = alloc_bytes(float_size - sizeof(long), TYPE_FLOAT);
  } else {
    /* allocate an extra 4 bytes for padding */
    ptr = alloc_bytes((sizeof(struct double_float)), TYPE_FLOAT); 
    DEREF(ptr) = HEADER(ptr);	/* copy header 1 word forward */
    LHEADER(ptr) = TYPE_PADDING;	
    ptr = ptr + 4;		/* Double align pointer */
  }
  return(ptr);
}

/* Return object size in bytes (NOT including header bytes) */
unsigned long object_size(LP ptr) {
  long i;

  switch ((TAG(ptr) & TAG_MASK)) {
  case TYPE_END_OF_PAGE : return(0);
  case TYPE_CONS: return(sizeof(struct cons) - sizeof(long));
  case TYPE_PROCEDURE:
    if (HEADER(ptr) == FUNCALLABLE_INSTANCE_HEADER) {
      return(sizeof(struct funcallable_instance) - sizeof(long));
    } else {
      return(sizeof(struct procedure) - sizeof(long));
    }
  case TYPE_SYMBOL: return(sizeof(struct symbol) - sizeof(long));
  case TYPE_LINE_SYMBOL: return(sizeof(struct line_symbol) - sizeof(long));
  case TYPE_CHARACTER: return(sizeof(struct character) - sizeof(long));
  case TYPE_COMPLEX: return(sizeof(struct complex) - sizeof(long));
  case TYPE_RATIO: return(sizeof(struct ratio) - sizeof(long));
  case TYPE_FLOAT: return(sizeof(struct double_float) - sizeof(long));
  case TYPE_OE: return(LEN_FIELD(ptr) * sizeof(LP));
  case TYPE_STRUCTURE: return(LEN_FIELD(ptr) * sizeof(LP));
  case TYPE_BIGNUM: return(ROUND_TO_WORD(LEN_FIELD(ptr)));
  case TYPE_CLOSURE: return(sizeof(CLOSURE));
  case TYPE_FORWARDING_PTR: return(LEN_FIELD(ptr));

  case TYPE_SIMPLE_BIT_VECTOR:
    i = LEN_FIELD(ptr);
    i = (i / (sizeof(long) * 8)) +
        (((i & ((sizeof(long) * 8) - 1)) == 0) ? 0 : 1); /* words */
    //i = i << 2;			              /* bytes */
    i = i * (sizeof(long) / sizeof(char));
    return(CORRECT_ZERO_OBJECT_SIZE(i));

  case TYPE_SIMPLE_STRING:
    i = LEN_FIELD(ptr) + 1;	/* account for terminating #\Null */
    i = ROUND_TO_WORD(i);
    return(i);

  case TYPE_SIMPLE_SIGNED_8BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_8BIT_VECTOR:
    i = LEN_FIELD(ptr);
    i = ROUND_TO_WORD(i);
    return(CORRECT_ZERO_OBJECT_SIZE(i));    

  case TYPE_SIMPLE_SIGNED_16BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_16BIT_VECTOR:
    i = LEN_FIELD(ptr) * (sizeof(long) / sizeof(short));
    i = ROUND_TO_WORD(i);
    return(CORRECT_ZERO_OBJECT_SIZE(i));

  case TYPE_SIMPLE_SIGNED_32BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_32BIT_VECTOR:
  case TYPE_SIMPLE_VECTOR:
    i = LEN_FIELD(ptr) * sizeof(long);
    return(CORRECT_ZERO_OBJECT_SIZE(i));

  case TYPE_SIMPLE_FLOAT_VECTOR:
    i = LEN_FIELD(ptr) * sizeof(double);
    return(CORRECT_ZERO_OBJECT_SIZE(i));

  case TYPE_SIMPLE_BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_8BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_8BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_CHAR_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_16BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_16BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_32BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_32BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_PTR_MULTI_ARRAY:
  case TYPE_SIMPLE_FLOAT_MULTI_ARRAY:
    return(sizeof(struct simple_multi_array) - sizeof(long));

  case TYPE_COMPLEX_BIT_VECTOR:
  case TYPE_COMPLEX_SIGNED_8BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_8BIT_VECTOR:
  case TYPE_COMPLEX_CHAR_VECTOR:
  case TYPE_COMPLEX_SIGNED_16BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_16BIT_VECTOR:
  case TYPE_COMPLEX_SIGNED_32BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_32BIT_VECTOR:
  case TYPE_COMPLEX_PTR_VECTOR:
  case TYPE_COMPLEX_FLOAT_VECTOR:
    return(sizeof(struct complex_vector) - sizeof(long));

  case TYPE_COMPLEX_BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_8BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_8BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_CHAR_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_16BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_16BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_32BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_32BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_PTR_MULTI_ARRAY:
  case TYPE_COMPLEX_FLOAT_MULTI_ARRAY:
    return(sizeof(struct complex_multi_array) - sizeof(long));

  case TYPE_FOREIGN_PTR:
    return(sizeof(struct foreign_pointer) - sizeof(long));

  case TYPE_PADDING: return(0);
    
  case TYPE_VOID:
  default: printf("object_size error: unknown tag: %x\n",TAG(ptr));
    lisp_debug();
  }
}

unsigned long gc_call_count() {
  return(gc_count);
}

unsigned long kbytes_to_pages(long kbytes) {
  return((PAGE_POWER > 10) ?
	 (kbytes >> (PAGE_POWER - 10)) :
	 (kbytes << (10 - PAGE_POWER)));
}

void * mmap_heap(long bytes) {
  return(mmap(0,
	      bytes,
	      PROT_EXEC | PROT_READ | PROT_WRITE,
	      MAP_PRIVATE | MAP_ANONYMOUS,
	      0,
	      0));
}

void init_memory_allocator(long dynamic_kbytes, long static_kbytes) {
  long i;

  inside_gc_flag = 0;
  print_gc_messages_flag = 0;
  gc_count = 0;
  /* Not sure we should rely on this as a way to get a stack ptr, but... */
  stack_bottom = (unsigned long *) &i;	
  total_pages = kbytes_to_pages(dynamic_kbytes);
  static_total_pages = kbytes_to_pages(static_kbytes);
  current_generation = 1;
  next_generation = 1;
  pageinfo = (struct page_info *)
    malloc(sizeof(struct page_info) * (total_pages + 1));
  for (i = 0; i <= total_pages; i++) {
    pageinfo[i].generation = 0;
    pageinfo[i].contig_flag = 0;
    pageinfo[i].next_lock = UNSCANNED;
  }
  //  first_ptr = (LP) malloc(BYTES_PER_PAGE * (total_pages + 1));
  first_ptr = (LP) mmap_heap(BYTES_PER_PAGE * (total_pages + 1));
  if ((first_ptr == 0) || (first_ptr == (LP) -1) || (pageinfo == 0)) {
    printf("Cannot allocate enough memory\n");
    /* HEY! Should try to allocate less memory rather than exiting
       Put this in a common safe_malloc routine and use it consistently. */
    exit(1);
  }
  /* Round up to the nearest page */
  first_ptr = ROUND_TO_PAGE(first_ptr);
  last_page = total_pages - 1;
  frontier_limit_ptr = PAGE_TO_ADDRESS(total_pages);
  allocated_pages = 0;
  frontier_page = 0;
  frontier_ptr = 0;
  remaining_page_bytes = 0;

  static_first_ptr = (LP) malloc(BYTES_PER_PAGE * (static_total_pages + 1));
  static_first_ptr = ROUND_TO_PAGE(static_first_ptr);
  static_frontier_limit_ptr = static_first_ptr +
                              (static_total_pages * BYTES_PER_PAGE);
  static_frontier_ptr = static_first_ptr;
  static_remaining_page_bytes = static_total_pages * BYTES_PER_PAGE;
}

static char* unknown_tag_name = "unknown tag";

char* tag_name(long tag) {
  switch (tag & TAG_MASK) {
  case TYPE_SIMPLE_BIT_VECTOR:
    return("TYPE_SIMPLE_BIT_VECTOR");
  case TYPE_SIMPLE_SIGNED_8BIT_VECTOR:
    return("TYPE_SIMPLE_SIGNED_8BIT_VECTOR");
  case TYPE_SIMPLE_UNSIGNED_8BIT_VECTOR:
    return("TYPE_SIMPLE_UNSIGNED_8BIT_VECTOR");
  case TYPE_SIMPLE_STRING:
    return("TYPE_SIMPLE_STRING");
  case TYPE_SIMPLE_SIGNED_16BIT_VECTOR:
    return("TYPE_SIMPLE_SIGNED_16BIT_VECTOR");
  case TYPE_SIMPLE_UNSIGNED_16BIT_VECTOR:
    return("TYPE_SIMPLE_UNSIGNED_16BIT_VECTOR");
  case TYPE_SIMPLE_SIGNED_32BIT_VECTOR:
    return("TYPE_SIMPLE_SIGNED_32BIT_VECTOR");
  case TYPE_SIMPLE_UNSIGNED_32BIT_VECTOR:
    return("TYPE_SIMPLE_UNSIGNED_32BIT_VECTOR");
  case TYPE_SIMPLE_VECTOR:
    return("TYPE_SIMPLE_VECTOR");
  case TYPE_SIMPLE_FLOAT_VECTOR:
    return("TYPE_SIMPLE_FLOAT_VECTOR");
  case TYPE_SIMPLE_BIT_MULTI_ARRAY:
    return("TYPE_SIMPLE_BIT_MULTI_ARRAY");
  case TYPE_SIMPLE_SIGNED_8BIT_MULTI_ARRAY:
    return("TYPE_SIMPLE_SIGNED_8BIT_MULTI_ARRAY");
  case TYPE_SIMPLE_UNSIGNED_8BIT_MULTI_ARRAY:
    return("TYPE_SIMPLE_UNSIGNED_8BIT_MULTI_ARRAY");
  case TYPE_SIMPLE_CHAR_MULTI_ARRAY:
    return("TYPE_SIMPLE_CHAR_MULTI_ARRAY");
  case TYPE_SIMPLE_SIGNED_16BIT_MULTI_ARRAY:
    return("TYPE_SIMPLE_SIGNED_16BIT_MULTI_ARRAY");
  case TYPE_SIMPLE_UNSIGNED_16BIT_MULTI_ARRAY:
    return("TYPE_SIMPLE_UNSIGNED_16BIT_MULTI_ARRAY");
  case TYPE_SIMPLE_SIGNED_32BIT_MULTI_ARRAY:
    return("TYPE_SIMPLE_SIGNED_32BIT_MULTI_ARRAY");
  case TYPE_SIMPLE_UNSIGNED_32BIT_MULTI_ARRAY:
    return("TYPE_SIMPLE_UNSIGNED_32BIT_MULTI_ARRAY");
  case TYPE_SIMPLE_PTR_MULTI_ARRAY:
    return("TYPE_SIMPLE_PTR_MULTI_ARRAY");
  case TYPE_SIMPLE_FLOAT_MULTI_ARRAY:
    return("TYPE_SIMPLE_FLOAT_MULTI_ARRAY");
  case TYPE_COMPLEX_BIT_VECTOR:
    return("TYPE_COMPLEX_BIT_VECTOR");
  case TYPE_COMPLEX_SIGNED_8BIT_VECTOR:
    return("TYPE_COMPLEX_SIGNED_8BIT_VECTOR");
  case TYPE_COMPLEX_UNSIGNED_8BIT_VECTOR:
    return("TYPE_COMPLEX_UNSIGNED_8BIT_VECTOR");
  case TYPE_COMPLEX_CHAR_VECTOR:
    return("TYPE_COMPLEX_CHAR_VECTOR");
  case TYPE_COMPLEX_SIGNED_16BIT_VECTOR:
    return("TYPE_COMPLEX_SIGNED_16BIT_VECTOR");
  case TYPE_COMPLEX_UNSIGNED_16BIT_VECTOR:
    return("TYPE_COMPLEX_UNSIGNED_16BIT_VECTOR");
  case TYPE_COMPLEX_SIGNED_32BIT_VECTOR:
    return("TYPE_COMPLEX_SIGNED_32BIT_VECTOR");
  case TYPE_COMPLEX_UNSIGNED_32BIT_VECTOR:
    return("TYPE_COMPLEX_UNSIGNED_32BIT_VECTOR");
  case TYPE_COMPLEX_PTR_VECTOR:
    return("TYPE_COMPLEX_PTR_VECTOR");
  case TYPE_COMPLEX_FLOAT_VECTOR:
    return("TYPE_COMPLEX_FLOAT_VECTOR");
  case TYPE_COMPLEX_BIT_MULTI_ARRAY:
    return("TYPE_COMPLEX_BIT_MULTI_ARRAY");
  case TYPE_COMPLEX_SIGNED_8BIT_MULTI_ARRAY:
    return("TYPE_COMPLEX_SIGNED_8BIT_MULTI_ARRAY");
  case TYPE_COMPLEX_UNSIGNED_8BIT_MULTI_ARRAY:
    return("TYPE_COMPLEX_UNSIGNED_8BIT_MULTI_ARRAY");
  case TYPE_COMPLEX_CHAR_MULTI_ARRAY:
    return("TYPE_COMPLEX_CHAR_MULTI_ARRAY");
  case TYPE_COMPLEX_SIGNED_16BIT_MULTI_ARRAY:
    return("TYPE_COMPLEX_SIGNED_16BIT_MULTI_ARRAY");
  case TYPE_COMPLEX_UNSIGNED_16BIT_MULTI_ARRAY:
    return("TYPE_COMPLEX_UNSIGNED_16BIT_MULTI_ARRAY");
  case TYPE_COMPLEX_SIGNED_32BIT_MULTI_ARRAY:
    return("TYPE_COMPLEX_SIGNED_32BIT_MULTI_ARRAY");
  case TYPE_COMPLEX_UNSIGNED_32BIT_MULTI_ARRAY:
    return("TYPE_COMPLEX_UNSIGNED_32BIT_MULTI_ARRAY");
  case TYPE_COMPLEX_PTR_MULTI_ARRAY:
    return("TYPE_COMPLEX_PTR_MULTI_ARRAY");
  case TYPE_COMPLEX_FLOAT_MULTI_ARRAY:
    return("TYPE_COMPLEX_FLOAT_MULTI_ARRAY");
  case TYPE_BIGNUM:
    return("TYPE_BIGNUM");
  case TYPE_RATIO:
    return("TYPE_RATIO");
  case TYPE_FLOAT:
    return("TYPE_FLOAT");
  case TYPE_COMPLEX:
    return("TYPE_COMPLEX");
  case TYPE_SYMBOL:
    return("TYPE_SYMBOL");
  case TYPE_LINE_SYMBOL:
    return("TYPE_LINE_SYMBOL");
  case TYPE_CHARACTER:
    return("TYPE_CHARACTER");
  case TYPE_PROCEDURE:
    return("TYPE_PROCEDURE");
  case TYPE_CONS:
    return("TYPE_CONS");
  case TYPE_STRUCTURE:
    return("TYPE_STRUCTURE");
  case TYPE_OE:
    return("TYPE_OE");
  case TYPE_VOID:
    return("TYPE_VOID");
  case TYPE_FOREIGN_PTR:
    return("TYPE_FOREIGN_PTR");
  case TYPE_CLOSURE:
    return("TYPE_CLOSURE");
  case TYPE_FORWARDING_PTR:
    return("TYPE_FORWARDING_PTR");
  case TYPE_PADDING:
    return("TYPE_PADDING");
  default: return(unknown_tag_name);
  }
}

void check_memory(long start_page, long end_page, long verbose) {
  long page,current_page,len,tag;
  char* name;
  LP ptr;
  LP next;
  LP tmp;

  if (pageinfo[start_page].contig_flag != 0) {
    printf("Cannot start memory check on a contiguous page, searching back\n");
    while (pageinfo[start_page].contig_flag != 0) {
      start_page = start_page - 1;
    }
  }
  page = start_page;  
  ptr = PAGE_TO_ADDRESS(page) + (sizeof(long) + 1);
  while ((page <= end_page) &&
	 (verbose || (page != ADDRESS_TO_PAGE(frontier_ptr)))) {
    if (verbose) {
      printf("***** page: %d, gen: %d, contig: %d, next: %d *****\n",
	     page,
	     pageinfo[page].generation,
	     pageinfo[page].contig_flag,
	     pageinfo[page].next_lock);
    }
    if ((pageinfo[page].generation != current_generation) &&
	(pageinfo[page].generation != next_generation)) {
      if (verbose) {
	printf("skipping old generation\n");
      }
      page = NEXT_PAGE(page);
      ptr = PAGE_TO_ADDRESS(page) + (sizeof(long) + 1);
    } else {
      current_page = page;
      while (current_page == page) {
	if (HEADER(ptr) == TYPE_END_OF_PAGE) {
	  if (verbose) {
	    printf("ptr %p, END_OF_PAGE header\n",ptr);
	  }
	  tmp = ptr - (sizeof(long) + 1);
	  ptr = ROUND_TO_PAGE(tmp) + (sizeof(long) + 1);
	  page = page + 1;
	} else {
	  len = object_size(ptr);
	  tag = TAG(ptr);
	  name = tag_name(tag);
	  if (name == unknown_tag_name) {
	    printf("unknown tag in heap\n");
	  }
	  if (verbose) {
	    printf("ptr %p, tag: %s (%x) rest: (%d), len: %d\n",
		   ptr,name,tag,LEN_FIELD(ptr),len);
	  }
	  ptr = ptr + len + sizeof(long);
	  tmp = ptr - (sizeof(long) + 1);
	  page = ADDRESS_TO_PAGE(tmp);
	}
      }
    }
  }
}

void intern_symbols(LP package_name, unsigned long *symbols) {
  unsigned long* syms;
  LP package;

  /* HEY! should just use c_to_lisp_string and let MAKE-PACKAGE coerce
     coerce to simple-string */
  package = p_lsp_FIND_2DOR_2DMAKE_2DPACKAGE(1,copy_c_to_lisp_string(package_name));
  for (syms = symbols; *syms != 0; syms = syms + 1) {
    p_lsp_ADD_2DSYMBOL(2, (LP) *syms,package);
  }
}

void register_symbols(char *package_name, unsigned long *symbols) {
  SYMBOL_RECORD *ptr;

  ptr = (SYMBOL_RECORD *) malloc(sizeof(SYMBOL_RECORD));
  ptr->package_name = package_name;
  ptr->symbols = symbols;
  if (!delay_symbol_interns) {
    intern_symbols(package_name, symbols);
  }
  ptr->next = registered_symbols;
  registered_symbols = ptr;
}


void intern_static_symbols() {
  SYMBOL_RECORD *ptr;

  for (ptr = registered_symbols; ptr != 0; ptr = ptr->next) {
    intern_symbols(ptr->package_name, ptr->symbols);
  }
  delay_symbol_interns = 0;
}

void scan_static_symbols() {
  SYMBOL_RECORD *ptr;
  unsigned long *syms;

  for (ptr = registered_symbols; ptr != 0; ptr = ptr->next) {
    for (syms = ptr->symbols; *syms != 0; syms = syms + 1) {
      move_sub_objects((LP) *syms);
    }
  }
}

long scan_memory_segment(unsigned long *low,
			 unsigned long *high,
			 char *segment_name) {
  unsigned long *ptr,*heap_ptr;
  long page,page_lock_count;

  page_lock_count = 0;
  ptr = low;
  while (ptr <= high) {
    heap_ptr = (unsigned long *) *ptr;
    if IN_HEAP(heap_ptr) {
      page = ADDRESS_TO_PAGE((LP) heap_ptr);
      if (pageinfo[page].generation == current_generation) {
	/* promote page(s) */
	if ((pageinfo[page].contig_flag != 0) && DEBUG_GC) {
	  printf("Counting locked page back to contig start\n");
	}
	while (pageinfo[page].contig_flag != 0) {
	  page = page - 1;
	  if (pageinfo[page].generation != current_generation) {
	    printf("Internal error - contig gen mismatch");
	  }
	}
	if (DEBUG_GC) {
	  printf("%d ",page);
	}
	pageinfo[page].generation = next_generation;
	page_lock_count = page_lock_count + 1;
	pageinfo[page].next_lock = lock_list;
	lock_list = page;
	/* promote all contig pages, but don't add them to the lock list */
	page = NEXT_PAGE(page);
	while ((pageinfo[page].contig_flag != 0) &&
	       (pageinfo[page].generation == current_generation)) {
	  pageinfo[page].generation = next_generation;
	  pageinfo[page].next_lock = CONTIG_LOCK;
	  page_lock_count = page_lock_count + 1;
	  page = NEXT_PAGE(page);
	}
      }
    }
    ptr = ptr + 1;
  }
  if (print_gc_messages_flag) {
    printf("%d Page locks from %s segment\n",page_lock_count,segment_name);
  }
  allocated_pages = allocated_pages + page_lock_count;
  return(page_lock_count);
}

void scan_stack() {
  long regs[NUMBER_OF_REGS_TO_SCAN];
  unsigned long *low;

  copy_regs_to_stack(&(regs[0]));
  low = (unsigned long *) &low;
  scan_memory_segment(low,stack_bottom,"Stack");
}

LP move_object(LP ptr) {
  long page = ADDRESS_TO_PAGE(ptr);
  if (FIXNUMP(ptr) ||
      !(IN_HEAP(ptr)) ||
      pageinfo[page].generation != current_generation) {
    return(ptr);
  } else {
    if ((TAG(ptr)) == TYPE_FORWARDING_PTR) {
      return((LP) DEREF(ptr));
    } else {
      long size = object_size(ptr);
      LP new_ptr;
      long i;


      /* Hey! Debugging code */
      if ((TAG(ptr)) == TYPE_PADDING) {
	printf("ERROR! Attempt to move a padding object\n");
	lisp_debug();
      }


      if (TAG(ptr) == TYPE_FLOAT) {
	new_ptr = alloc_float();
      } else {
	new_ptr = alloc_bytes(size,TAG(ptr));
      }

      for (i = -1 * sizeof(long); i < size; i = i + sizeof(long)) {
	DEREF(new_ptr + i) = DEREF(ptr + i);
      }
      LHEADER(ptr) = (size << 8) + TYPE_FORWARDING_PTR;
      DEREF(ptr) = (LD) new_ptr;
      return(new_ptr);
    }
  }
}

LP move_object_unless_locked(LP ptr) {
  long page = ADDRESS_TO_PAGE(ptr);
  
  if (PAGE_LOCKED(page)) {
    return(ptr);	
  } else {
    return(move_object(ptr));
  }
}

void scan_misc_static_pointers() {
  if (IN_HEAP(OE)) {
    OE = move_object_unless_locked(OE);
  }
}

void scan_static_space() {
  LP ptr;
  ptr = static_first_ptr + (sizeof(long) + 1);
  while (ptr < static_frontier_ptr) {
    move_sub_objects(ptr);
    ptr = ptr + object_size(ptr) + sizeof(long);
  }
}

void scan_root_set() {
  lock_list = END_LOCK_LIST;
  scan_stack();
  scan_static_symbols();
  scan_static_space();
  scan_misc_static_pointers();
}

void move_sub_objects(LP ptr) {
  switch (TAG(ptr) & TAG_MASK) {
  case TYPE_FORWARDING_PTR:
    printf("Error to move sub_objects of fwd ptr! \n");
    lisp_debug();

  case TYPE_CONS:
    LDREF(ptr,CONS,car) = move_object(LDREF(ptr,CONS,car));
    LDREF(ptr,CONS,cdr) = move_object(LDREF(ptr,CONS,cdr));
    break;

  case TYPE_PROCEDURE:
    switch (HEADER(ptr)) {
    case FUNCALLABLE_INSTANCE_HEADER:
      LDREF(ptr,FUNCALLABLE_INSTANCE,code_pointer) =
	REMOVE_TAG(move_object(ADD_TAG(LDREF(ptr,FUNCALLABLE_INSTANCE,
					     code_pointer))));
      LDREF(ptr,FUNCALLABLE_INSTANCE,wrapper) =
	move_object(LDREF(ptr,FUNCALLABLE_INSTANCE,wrapper));
      LDREF(ptr,FUNCALLABLE_INSTANCE,slots) =
	move_object(LDREF(ptr,FUNCALLABLE_INSTANCE,slots));
      break;
    case CLOSED_PROCEDURE_HEADER:
      LDREF(ptr,PROCEDURE,code_pointer) =
	REMOVE_TAG(move_object(ADD_TAG(LDREF(ptr,PROCEDURE,code_pointer))));
      break;
    }
    break;  

  case TYPE_CLOSURE:

#ifdef x86
#if (__WORDSIZE == 64)
    {
      unsigned int old_oe_low32;
      unsigned long old_oe_high32;
      unsigned long old_oe, new_oe;

      old_oe_low32 = CLOSURE_ENV_LOW32((ptr - 1));
      old_oe_high32 = CLOSURE_ENV_HIGH32((ptr - 1));
      old_oe = (old_oe_high32 << 32) | old_oe_low32;
      new_oe = (unsigned long) move_object((LP) old_oe);
      CLOSURE_ENV_LOW32((ptr - 1)) = (unsigned int) new_oe;
      CLOSURE_ENV_HIGH32((ptr - 1)) = (unsigned int) (new_oe >> 32);
    }
#else
    /* x86 32 bit code */
    {
        unsigned long old_oe, new_oe;

	old_oe = CLOSURE_ENV((ptr - 1));
	new_oe = (unsigned long) move_object(old_oe);
	CLOSURE_ENV((ptr - 1)) = new_oe;
    }
#endif
#endif

#ifdef SPARC
    {
        unsigned long hi22,low10,old_oe,new_oe;

	hi22 = ((CLOSURE *) (ptr - 1))->sethi_oe;
	low10 = ((CLOSURE *) (ptr - 1))->add_oe;
	old_oe = (hi22 << 10) + low10;
	new_oe = (unsigned long) move_object(old_oe);
	
	((CLOSURE *) (ptr - 1))->sethi_oe = HI22(new_oe);
	((CLOSURE *) (ptr - 1))->add_oe = LOW10(new_oe);
	/* Synch i+d caches if they're separate
	   This flushes the entire closure, really only need to flush
	   the part we just changed. */
	flush_cache_addresses((ptr - 1), sizeof(CLOSURE));
     }
#endif
    break;

  case TYPE_COMPLEX:
    LDREF(ptr,COMPLEX,real) = move_object(LDREF(ptr,COMPLEX,real));
    LDREF(ptr,COMPLEX,imaginary) = move_object(LDREF(ptr,COMPLEX,imaginary));
    break;

  case TYPE_RATIO:
    LDREF(ptr,RATIO,numerator) = move_object(LDREF(ptr,RATIO,numerator));
    LDREF(ptr,RATIO,denominator) = move_object(LDREF(ptr,RATIO,denominator));
    break;

  case TYPE_SYMBOL:
    LDREF(ptr,SYMBOL,self_link) = ptr;
    LDREF(ptr,SYMBOL,value) = move_object(LDREF(ptr,SYMBOL,value));
    LDREF(ptr,SYMBOL,package) = move_object(LDREF(ptr,SYMBOL,package));
    LDREF(ptr,SYMBOL,name) = move_object(LDREF(ptr,SYMBOL,name));
    LDREF(ptr,SYMBOL,plist) = move_object(LDREF(ptr,SYMBOL,plist));
    LDREF(ptr,SYMBOL,function) = move_object(LDREF(ptr,SYMBOL,function));
    break;

  case TYPE_LINE_SYMBOL:
    LDREF(ptr,LINE_SYMBOL,self_link) =
      move_object(LDREF(ptr,LINE_SYMBOL,self_link));
    break;

  case TYPE_OE:
  case TYPE_STRUCTURE:
  case TYPE_SIMPLE_VECTOR:
    {
      long i;
      long len = LEN_FIELD(ptr) * sizeof(long);
      for (i = 0; i < len; i = i + sizeof(long)) {
	DEREF(ptr + i) = (LD) move_object((LP) DEREF(ptr + i));
      }
    }
    break;

  case TYPE_BIGNUM:
    LDREF(ptr, BIGNUM_HEADER, bits_ptr) = &(LDREF(ptr, BIGNUM, bits[0]));
    break;

  case TYPE_CHARACTER:
  case TYPE_FLOAT:
  case TYPE_SIMPLE_BIT_VECTOR:
  case TYPE_SIMPLE_SIGNED_8BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_8BIT_VECTOR:
  case TYPE_SIMPLE_STRING:
  case TYPE_SIMPLE_SIGNED_16BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_16BIT_VECTOR:
  case TYPE_SIMPLE_SIGNED_32BIT_VECTOR:
  case TYPE_SIMPLE_UNSIGNED_32BIT_VECTOR:
  case TYPE_SIMPLE_FLOAT_VECTOR:
  case TYPE_PADDING:
    break;


  case TYPE_SIMPLE_BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_8BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_8BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_CHAR_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_16BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_16BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_SIGNED_32BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_UNSIGNED_32BIT_MULTI_ARRAY:
  case TYPE_SIMPLE_PTR_MULTI_ARRAY:
  case TYPE_SIMPLE_FLOAT_MULTI_ARRAY:
    LDREF(ptr,SIMPLE_MULTI_ARRAY,underlying_vector) =
      move_object(LDREF(ptr,SIMPLE_MULTI_ARRAY,underlying_vector));
    LDREF(ptr,SIMPLE_MULTI_ARRAY,dims_vector) =
      move_object(LDREF(ptr,SIMPLE_MULTI_ARRAY,dims_vector));
    LDREF(ptr,SIMPLE_MULTI_ARRAY,multiplier_vector) =
      move_object(LDREF(ptr,SIMPLE_MULTI_ARRAY,multiplier_vector));
    break;

  case TYPE_COMPLEX_BIT_VECTOR:
  case TYPE_COMPLEX_SIGNED_8BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_8BIT_VECTOR:
  case TYPE_COMPLEX_CHAR_VECTOR:
  case TYPE_COMPLEX_SIGNED_16BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_16BIT_VECTOR:
  case TYPE_COMPLEX_SIGNED_32BIT_VECTOR:
  case TYPE_COMPLEX_UNSIGNED_32BIT_VECTOR:
  case TYPE_COMPLEX_PTR_VECTOR:
  case TYPE_COMPLEX_FLOAT_VECTOR:
    LDREF(ptr,COMPLEX_VECTOR,underlying_vector) =
      move_object(LDREF(ptr,COMPLEX_VECTOR,underlying_vector));
    /* Ignore fill pointer and displaced-index offset */
    break;

  case TYPE_COMPLEX_BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_8BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_8BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_CHAR_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_16BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_16BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_SIGNED_32BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_UNSIGNED_32BIT_MULTI_ARRAY:
  case TYPE_COMPLEX_PTR_MULTI_ARRAY:
  case TYPE_COMPLEX_FLOAT_MULTI_ARRAY:
    LDREF(ptr,COMPLEX_MULTI_ARRAY,underlying_vector) =
      move_object(LDREF(ptr,COMPLEX_MULTI_ARRAY,underlying_vector));
    LDREF(ptr,COMPLEX_MULTI_ARRAY,dims_vector) =
      move_object(LDREF(ptr,COMPLEX_MULTI_ARRAY,dims_vector));
    LDREF(ptr,COMPLEX_MULTI_ARRAY,multiplier_vector) =
      move_object(LDREF(ptr,COMPLEX_MULTI_ARRAY,multiplier_vector));
    /* Ignore displaced-index offset */
    break;

  case TYPE_FOREIGN_PTR:
    /* DEREF(ptr) = (LD) move_object(DEREF(ptr)); */
    break;

  case TYPE_VOID:
  default:
    printf("move_sub_object error: unknown tag: %x\n",TAG(ptr));
    lisp_debug();
  }
}

/* Return next page, or -1 if done */
long scan_page(long page) {
  LP ptr;
  LP end_ptr;
  long next_page;

  if (pageinfo[page].contig_flag == 0) {
    ptr = PAGE_TO_ADDRESS(page) + (sizeof(long) + 1);
  } else {
    long start_page = page;
    while (pageinfo[start_page].contig_flag != 0) {
      start_page = start_page - 1;
    }
    ptr = PAGE_TO_ADDRESS(start_page) + sizeof(long) + 1;
    ptr = ptr + object_size(ptr) + sizeof(long);
  }
  end_ptr = ROUND_TO_PAGE(ptr + object_size(ptr) - 1);
  
  if (DEBUG_GC) {
    printf("scan page %d, ",page);
  }
  /* Scan page and any contig pages */
  while (1) {
    if (ptr == frontier_ptr + sizeof(long) + 1) {
      return(-1);
    }
    if ((HEADER(ptr) == TYPE_END_OF_PAGE) || (ptr >= end_ptr)) {
      next_page = (NEXT_PAGE(ADDRESS_TO_PAGE(end_ptr - 1)));
      if (next_page > last_page) {
	printf("Internal page scanning error\n");
	lisp_debug();
      } else {
	if (DEBUG_GC) {
	  printf("next page: %d\n",next_page);
	}
	return(next_page);
      }
    }
    move_sub_objects(ptr);
    ptr = ptr + object_size(ptr) + sizeof(long);
  }
}

void scan_pages(long start_page) {
  long page;
  LP ptr;

  page = start_page;
  while (lock_list != END_LOCK_LIST) {
    scan_page(lock_list);
    lock_list = pageinfo[lock_list].next_lock;
  }
  while (page != -1) {
    if (PAGE_UNLOCKED(page)) {
      page = scan_page(page);
    } else {
      page = NEXT_PAGE(page);
    }
  }
}

#if RTGC
void full_gc() {
  RTfull_gc();
}
#else
void full_gc() {
  long start_page; 
  struct timeval start_tv, end_tv, elapsed_tv;
  
  gettimeofday(&start_tv, 0);
  if (inside_gc_flag != 0) {
    printf("\nGC called recursively, must be out of memory.\n");
    lisp_debug();
  } 

  inside_gc_flag = 1;    
  if (print_gc_messages_flag) {
    printf("GC called. Frontier page = %d\n",frontier_page);
  }

  terminate_page();
  next_generation = current_generation + 1;
  allocated_pages = 0;
  start_page = frontier_page;

  scan_root_set();
  scan_pages(start_page);
  current_generation = next_generation;
  inside_gc_flag = 0;
  gc_count = gc_count + 1;
  gettimeofday(&end_tv, 0);
  timersub(&end_tv, &start_tv, &elapsed_tv);
  printf("GC elapsed_tv is %d.%06d\n", elapsed_tv.tv_sec, elapsed_tv.tv_usec);
  printf("GC done, checking memory...");
  check_memory(0,frontier_page,0);
  printf("done\n\n");
}
#endif
