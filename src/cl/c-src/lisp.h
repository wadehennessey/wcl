/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#define linux_pc

#ifdef linux_pc
#define x86
#endif

#define RTGC 1

#include <stdarg.h>
#include <setjmp.h> 
//#include <stdlib.h>

/* 
TAG byte layout:

bit7  bit6   bit5   bit4  bit3   bit2         bit1        bit0
-------------------------------------------------------------------
fillp <array element type....>   0: simple    0: vector   0: array
                                 1: complex   1: multi

deref <bignum, ratio tags>       0: rational  0: number   1: other
      <float, complex tags>      1: other-num

deref <real, line symbol tags>   0: symbol    1: other    1: other
deref <enum other type.... >     1: other
*/

#define ARRAY_TAG                                      0x0
#define OTHER_TAG                                      0x1
#define VECTOR_TAG                                     0x0
#define MULTI_ARRAY_TAG                                0x2
#define NUMBER_TAG                                     0x1
#define TAG_MASK                                       0x7f

#define VECTOR_TAG_MASK                                0x3
#define NUMBER_TAG_MASK                                0x3

#define VECTOR_MASK                                    0x7B
#define ARRAY_MASK                                     0x79

#define SIMPLE_VECTOR_TAG                              0x0
#define SIMPLE_MULTI_ARRAY_TAG                         0x2
#define COMPLEX_VECTOR_TAG                             0x4        
#define COMPLEX_MULTI_ARRAY_TAG                        0x6

#define ARRAY_ELEMENT_TYPE_MASK                        0x78
#define ELEMENT_TYPE_BIT                               0x00
#define ELEMENT_TYPE_SIGNED_8BIT                       0x08
#define ELEMENT_TYPE_UNSIGNED_8BIT                     0x10
#define ELEMENT_TYPE_CHAR                              0x18
#define ELEMENT_TYPE_SIGNED_16BIT                      0x20
#define ELEMENT_TYPE_UNSIGNED_16BIT                    0x28
#define ELEMENT_TYPE_SIGNED_32BIT                      0x30
#define ELEMENT_TYPE_UNSIGNED_32BIT                    0x38
#define ELEMENT_TYPE_PTR                               0x40
#define ELEMENT_TYPE_FLOAT                             0x48

#define TYPE_SIMPLE_BIT_VECTOR                         0x00
#define TYPE_SIMPLE_SIGNED_8BIT_VECTOR                 0x08
#define TYPE_SIMPLE_UNSIGNED_8BIT_VECTOR               0x10
#define TYPE_SIMPLE_STRING                             0x18
#define TYPE_SIMPLE_SIGNED_16BIT_VECTOR                0x20
#define TYPE_SIMPLE_UNSIGNED_16BIT_VECTOR              0x28
#define TYPE_SIMPLE_SIGNED_32BIT_VECTOR                0x30
#define TYPE_SIMPLE_UNSIGNED_32BIT_VECTOR              0x38
#define TYPE_SIMPLE_VECTOR                             0x40
#define TYPE_SIMPLE_FLOAT_VECTOR                       0x48

#define TYPE_SIMPLE_BIT_MULTI_ARRAY                    0x02
#define TYPE_SIMPLE_SIGNED_8BIT_MULTI_ARRAY            0x0a
#define TYPE_SIMPLE_UNSIGNED_8BIT_MULTI_ARRAY          0x12
#define TYPE_SIMPLE_CHAR_MULTI_ARRAY                   0x1a
#define TYPE_SIMPLE_SIGNED_16BIT_MULTI_ARRAY           0x22
#define TYPE_SIMPLE_UNSIGNED_16BIT_MULTI_ARRAY         0x2a
#define TYPE_SIMPLE_SIGNED_32BIT_MULTI_ARRAY           0x32
#define TYPE_SIMPLE_UNSIGNED_32BIT_MULTI_ARRAY         0x3a
#define TYPE_SIMPLE_PTR_MULTI_ARRAY                    0x42
#define TYPE_SIMPLE_FLOAT_MULTI_ARRAY                  0x4a


#define TYPE_COMPLEX_BIT_VECTOR                        0x04
#define TYPE_COMPLEX_SIGNED_8BIT_VECTOR                0x0c
#define TYPE_COMPLEX_UNSIGNED_8BIT_VECTOR              0x14
#define TYPE_COMPLEX_CHAR_VECTOR                       0x1c
#define TYPE_COMPLEX_SIGNED_16BIT_VECTOR               0x24
#define TYPE_COMPLEX_UNSIGNED_16BIT_VECTOR             0x2c
#define TYPE_COMPLEX_SIGNED_32BIT_VECTOR               0x34
#define TYPE_COMPLEX_UNSIGNED_32BIT_VECTOR             0x3c
#define TYPE_COMPLEX_PTR_VECTOR                        0x44
#define TYPE_COMPLEX_FLOAT_VECTOR                      0x4c


#define TYPE_COMPLEX_BIT_MULTI_ARRAY                   0x06
#define TYPE_COMPLEX_SIGNED_8BIT_MULTI_ARRAY           0x0e
#define TYPE_COMPLEX_UNSIGNED_8BIT_MULTI_ARRAY         0x16
#define TYPE_COMPLEX_CHAR_MULTI_ARRAY                  0x1e
#define TYPE_COMPLEX_SIGNED_16BIT_MULTI_ARRAY          0x26
#define TYPE_COMPLEX_UNSIGNED_16BIT_MULTI_ARRAY        0x2e
#define TYPE_COMPLEX_SIGNED_32BIT_MULTI_ARRAY          0x36
#define TYPE_COMPLEX_UNSIGNED_32BIT_MULTI_ARRAY        0x3e
#define TYPE_COMPLEX_PTR_MULTI_ARRAY                   0x46
#define TYPE_COMPLEX_FLOAT_MULTI_ARRAY                 0x4e


#define TYPE_BIGNUM                                    0x01
#define TYPE_RATIO                                     0x09
#define TYPE_FLOAT                                     0x05
#define TYPE_COMPLEX                                   0x0d

#define TYPE_SYMBOL                                    0x03
#define TYPE_LINE_SYMBOL                               0x0b

#define TYPE_CHARACTER                                 0x07
#define TYPE_CONS                                      0x0f
#define TYPE_OE                                        0x17
#define TYPE_FOREIGN_PTR                               0x1f
#define TYPE_PROCEDURE                                 0x27
#define TYPE_STRUCTURE                                 0x2f
#define TYPE_VOID                                      0x37
#define TYPE_CLOSURE                                   0x3f
#define TYPE_FORWARDING_PTR                            0x47
#define TYPE_UBV                                       0x4f
#define TYPE_PADDING                                   0x57
#define TYPE_END_OF_PAGE                               0x7f

#define LP unsigned char *
#define LPL unsigned long *
#define LREF(x) (((LP) &(x)) + sizeof(long *) + 1) /* skip header, add tag */
#define DEREF(x) *((LPL) ((x) - 1))     /* adjust for tag, then ref */
#define ADD_TAG(x) ((LP) (x) + 1)
#define REMOVE_TAG(x) ((LP) (x) - 1)
#define LD unsigned long
#define LDREF(ptr,type,field) ((type *) (((LP) ptr) - sizeof(long *) - 1))->field
#define SYMREF(sym,slot) LDREF((LDREF(sym,SYMBOL,self_link)),SYMBOL,slot)
#define NULL 0
#define NIL LREF(s_lsp_NIL)
#define T LREF(s_lsp_T)
#define UBV_MARKER LREF(ubv_marker)
#define UBK_MARKER LREF(ubv_marker)

#define MOST_POSITIVE_FIXNUM ((1L << ((sizeof(long) * 8) - 2)) - 1)
#define MOST_NEGATIVE_FIXNUM (-1 * (MOST_POSITIVE_FIXNUM + 1))

#define FX_TO_LONG(fx) ((long) (fx) >> 1)
#define FX_TO_ULONG(fx) ((long) (fx) >> 1) /*  HEY! fix this... */
#define LONG_TO_FX(i)  ((LP) ((long) (i) << 1))
#define LONG_TO_INTEGER(i) (((((long) (i)) <= MOST_POSITIVE_FIXNUM) && \
			      (((long) (i)) >= MOST_NEGATIVE_FIXNUM)) ? \
			     LONG_TO_FX(i) : \
			     long_to_bignum(i))
#define ULONG_TO_INTEGER(i) ((((unsigned long) (i)) \
			       <= MOST_POSITIVE_FIXNUM) ? \
			     LONG_TO_FX(i) : \
			     ulong_to_bignum((unsigned long) (i)))
#define INTEGER_TO_LONG(i) (FIXNUMP(i) ? FX_TO_LONG(i) : bignum_to_long(i))
#define INTEGER_TO_ULONG(i) (FIXNUMP(i) ? FX_TO_ULONG(i) : bignum_to_ulong(i))

#define FIXNUMP(x) (((unsigned long) (x) & 1) == 0) 
#define OTHER_PTRP(x) (((unsigned long) (x) & 1) != 0)

#define HEADER(obj) ((unsigned long) DEREF((obj) - sizeof(long)))
#define LHEADER(obj) DEREF((obj) - sizeof(long)) 
#define TAG(obj) ((unsigned char) HEADER(obj))
#define LEN_FIELD(obj) (HEADER(obj) >> 8)
#define PASS_TO_C(obj) (FIXNUMP(obj) ? FX_TO_LONG(obj) : \
                        (IMMED_OBJ_P(obj) ? \
			(unsigned long) DEREF(obj) : \
			(unsigned long) (obj - 1)))
#define LISTP(x) (((OTHER_PTRP(x) && (TAG(x) == TYPE_CONS))) || (x == NIL))

#define NEW_PROCEDURE alloc_words(1,TYPE_PROCEDURE)
#define NEW_CONS      alloc_words(2,TYPE_CONS)

#define PTR_TO_INT(p) ((long)(p) & 0xFFFFFFFF)

/* Closure allocation. */

#define OPEN_PROCEDURE_FLAG 1
#define CLOSED_PROCEDURE_FLAG 1234
#define FUNCALLABLE_INSTANCE_FLAG 5678
#define FUNCALLABLE_INSTANCE_HEADER ((FUNCALLABLE_INSTANCE_FLAG << 8) + \
				     TYPE_PROCEDURE)
#define CLOSED_PROCEDURE_HEADER ((CLOSED_PROCEDURE_FLAG << 8) + \
				 TYPE_PROCEDURE)
#define NEW_OE(len) new_oe(len)
#define GET_OE_SLOT(oe_var,i) (LP) (DEREF(oe_var + i * sizeof(long *)))
//#define SET_OE_SLOT(oe_var,i,value) (DEREF(oe_var + i * sizeof(long *)) = (LD) value)

#define SET_OE_SLOT(oe_var,i,value) wcl_wb(&(DEREF(oe_var + i * sizeof(long *))),(LP) value)

#define MAKE_CLOSURE(code,env) new_closure((LP) code,env)
#define COERCE_TO_FUNCTION(x) p_lsp_COERCE_2DTO_2DFUNCTION(1,x)

#define ICALL(sym) ((LP (*)(ARGC dummy, ...)) ((LP) DEREF(sym.function)))
#define CODE_PTR(procedure) ((LP (*)(ARGC dummy, ...)) ((LP) DEREF(procedure)))

/* object layout */

typedef struct double_float {
  unsigned long header;
  double number;
} FLOAT;

typedef struct ratio {
  unsigned long header;
  LP numerator;
  LP denominator;
} RATIO;

typedef struct complex {
  unsigned long header;
  LP real;
  LP imaginary;
} COMPLEX;

typedef  struct character  {
  unsigned long header;
  unsigned int char_code;
} CHARACTER;

/* The length field of the header holds a number which indicates if this
   procedure is a closure or a funcallable instance. */
typedef struct procedure {
  unsigned long header;
  unsigned char *code_pointer;
} PROCEDURE;

typedef struct funcallable_instance {
  unsigned long header;
  unsigned char *code_pointer;
  LP wrapper;
  LP slots;
} FUNCALLABLE_INSTANCE;

typedef struct symbol {
  unsigned long header;
  LP value;
  LP package;
  LP self_link;			/* only needed if we use line_symbols */
  LP plist;
  LP function;
  LP hashcode;
  unsigned long flags;
  LP name;
} SYMBOL;

/* Experimental line number debugging hack. Probably a loser. */
typedef struct line_symbol {
  unsigned long header;
  LP line;
  LP padding;			/* preserve car/cdr hack */
  LP self_link; 
} LINE_SYMBOL;

typedef struct cons {
  unsigned long header;
  LP car;
  LP cdr;
} CONS;

typedef struct simple_multi_array {
  unsigned long header;
  LP underlying_vector;
  LP dims_vector;
  LP multiplier_vector;
} SIMPLE_MULTI_ARRAY;

typedef struct complex_vector {
  unsigned long header;
  LP underlying_vector;
  LP fill_pointer;
  LP displaced_index_offset;
} COMPLEX_VECTOR;

typedef struct complex_multi_array {
  unsigned long header;
  LP underlying_vector;
  LP dims_vector;
  LP multiplier_vector;
  LP displaced_index_offset;
} COMPLEX_MULTI_ARRAY;

typedef struct ubv {
  unsigned long header;
  unsigned long pad;
} UBV;

typedef struct foreign_pointer {
  unsigned long header;
  LP pointer;
  LP type;
} FOREIGN_POINTER;

/* This is the wcl version of MP_INT in gmp.h */
typedef struct bignum_header {
  unsigned long header;
  int alloced_size;
  int used_size;		/* negative if bignum is negative */
  long *bits_ptr;		/* Points to bits immediately following */
} BIGNUM_HEADER;

typedef struct bignum {
  BIGNUM_HEADER header;
  long bits[1];			/* bits len is variable in reality */
} BIGNUM;

/* Some losing C preprocessors will only pass 80 chars in a string! */
#define MAKE_SIMPLE_STRING(label,len,str) \
  static struct {unsigned long header; char string[len+1];} \
  label  = {((len << 8) + TYPE_SIMPLE_STRING), str}

#define MAKE_SYMBOL(label,value,package,name,plist,function,hashcode,flags) \
  SYMBOL label = {TYPE_SYMBOL, value, package, LREF(label), \
		  plist, function, hashcode,flags, name}

#define MAKE_FLOAT(label,f) \
  static FLOAT label = {TYPE_FLOAT, f}

#define MAKE_RATIO(label,numerator,denominator) \
  static RATIO label = {TYPE_RATIO, numerator, denominator}

#define MAKE_COMPLEX(label,real,imag) \
  static COMPLEX label = {TYPE_COMPLEX, real, imag}

#define MAKE_CONS(label,car,cdr) \
  static CONS label = {TYPE_CONS,car,cdr}

#define MAKE_PROCEDURE(label,code_ptr) \
  static PROCEDURE label = {TYPE_PROCEDURE, (LP) code_ptr}

extern SYMBOL s_lsp_NIL;
extern SYMBOL s_lsp_T;
extern CHARACTER char_tab[];

#define NEW_FLOAT(expr) new_float((double) expr)
#define NEW_CHAR(expr) ((LP) LREF(char_tab[(long) expr]))
#define NEW_FPTR(type,expr) new_foreign_ptr((LP) type, (LP) expr)

/* We need this to cope with the alignment of doubles in structures */
#define RAW_FLOAT(x) (((FLOAT *) ((x) - sizeof(long) - 1))->number)
#define RAW_CHAR(x) (((CHARACTER *) ((x) - sizeof(long) - 1))->char_code)
#define RAW_FPTR(x) (((FOREIGN_POINTER *) ((x) - sizeof(long) - 1))->pointer)

/* Arg related stuff */
#define ARGC unsigned long
#define CALL_ARG_LIMIT 512
#define MULTIPLE_VALUE_LIMIT 512
#define APPLY_ARGS_LIMIT CALL_ARG_LIMIT	/* but Generic apply only handles 32 */

typedef struct mv {
  ARGC argc;
  long  return_flag;
  LP values[MULTIPLE_VALUE_LIMIT];
} MV;

#define MV_HOLDER_P(x) (((unsigned long) x) > 0xffff)

#define BEGIN_MV_CALL(mv_holder,real_argc) \
  { MV holder; \
    MV * mv_holder = &holder; \
    mv_holder->argc = real_argc; \
    mv_holder->return_flag = -1

#define MV_CALL(mv_holder,real_argc) \
    (MV_HOLDER_P(mv_holder) ? \
     (((MV *) mv_holder)->argc = real_argc, (ARGC) mv_holder) : real_argc)

#define SET_MV_RETURN_FLAG(mv_holder) ((MV *)mv_holder)->return_flag = 1

#define SET_MV_RETURN_COUNT(mv_holder,count) ((MV *)mv_holder)->argc = count

#define SET_MV_RETURN_VALUE(mv_holder,i,value) \
      ((MV *)mv_holder)->values[i] = value

#define GET_MV_RETURN_COUNT(mv_holder) ((MV *)mv_holder)->argc

#define MV_RETURN_P(mv_holder) (mv_holder->return_flag != -1)

#define SV_RETURN_P(mv_holder) (mv_holder->return_flag == -1)

#define END_MV_CALL \
  }

#define REAL_ARGC(mv_holder) \
  (MV_HOLDER_P(mv_holder) ? \
   ((MV *) mv_holder)->argc : mv_holder)

#define BEGIN_ANSI_VAR_ARGS(last_required) \
  { va_list ap; \
    va_start(ap,last_required)

#define NEXT_VAR_ARG va_arg(ap,LP)

#define END_VAR_ARGS \
  va_end(ap); \
  }

#define BEGIN_VAR_VALUES \
  { long index = 0		/*  don't confilict with next in RESTIFY! */
    
#define NEXT_VAR_VALUE(mv_holder) mv_holder->values[index++]

#define END_VAR_VALUES \
  }

#define DYNAMIC_REST_HOLDER(var) CONS var[CALL_ARG_LIMIT]

#define RESTV_HOLDER(var)  LP var[CALL_ARG_LIMIT + 1]

#define RESTIFY(rest_var,start_from,next_arg_func) \
    { ARGC start; LP tail = NIL; LP next;  \
      rest_var = NIL; \
      if (real_argc > CALL_ARG_LIMIT) arg_limit_exceeded(real_argc); \
      for (start = start_from; start <= real_argc; start++) { \
	next = NEW_CONS; \
	LDREF(next,CONS,car) = next_arg_func; \
	if (tail != NIL) LDREF(tail,CONS,cdr) = next; \
	if (rest_var == NIL) rest_var = next; \
	tail = next; \
      } \
      if (tail != NIL) LDREF(tail,CONS,cdr) = NIL; \
    }

#define DYNAMIC_RESTIFY(rest_var,start_from,next_arg_func) \
    { int len = (real_argc - start_from + 1); int i = 0; \
      if (len > CALL_ARG_LIMIT) arg_limit_exceeded(len); \
      if (len <= 0) { \
	rest_var = NIL; \
      } else { \
	while (i < len) { \
	 rest_conses[i].header = TYPE_CONS; \
	 rest_conses[i].car = next_arg_func; \
         rest_conses[i].cdr = ((LP) &(rest_conses[i + 1].car)) + 1; \
	 i = i + 1; \
        } \
        rest_conses[i - 1].cdr = NIL; \
        rest_var = ((LP) &(rest_conses[0].car)) + 1; \
      } \
    }

#define RESTVIFY(restv_var,start_from,next_arg_func) \
    { int len = (real_argc - start_from + 1); int i; \
      if (len > CALL_ARG_LIMIT) arg_limit_exceeded(len); \
    restv_vector[0] = (LP) (TYPE_SIMPLE_VECTOR + (len << 8)); \
    for (i = 1; i <= len; i++) restv_vector[i] =  next_arg_func; \
    restv_var = ((LP) &(restv_vector[1])) + 1; \
    }

#define BEGIN_KEY_INIT(var,keyword,rest) \
 if ((var = lookup_keyword(keyword,rest)) == UBK_MARKER) {	

#define END_KEY_INIT }

/* DYNAMIC unwind/protect stuff */
#define UW_CATCH        0
#define UW_PROTECT      1
#define UW_SPECBIND     2
#define UW_DYNAMIC_TAG  3

/* We could have different types  (CATCH, SPECBIND, PROTECT) of structures
   rather than one. Perhaps it's worth changing one day... */
typedef struct uw_point {
  struct uw_point *next;
  char type;
  MV *mv_holder;		/* for mv catch */
  LP single_throw_value;
  jmp_buf c_env;
  struct uw_point *continue_dest;
  LP name;
  LP value;
} UW_POINT;

extern UW_POINT *uw_top;

#define POP_UW_POINT uw_top = uw_top->next

#define PUSH_UW_POINT(new) new.next = uw_top; uw_top = &new 

#define BEGIN_CATCH(tag,values_holder) \
    { UW_POINT uwp; \
      LP catch_tmp; \
      uwp.type = UW_CATCH; \
      uwp.name = tag;  \
      uwp.mv_holder = (MV *) values_holder; \
      if  ((catch_tmp = (LP) setjmp(uwp.c_env)) == 0) { \
          PUSH_UW_POINT(uwp);

/* HEY! Setjmp converts 0 to 1! Have to convert back explicitly */
#define END_CATCH(value_var) \
      } else value_var = (catch_tmp == 1 ? 0 : uwp.single_throw_value); \
    POP_UW_POINT; \
    } 

#define BEGIN_SPEC_BIND(symbol,new_value) \
  { UW_POINT uwp; \
    uwp.type = UW_SPECBIND; \
    uwp.name = LREF(symbol); \
    uwp.value = symbol.value; \
    PUSH_UW_POINT(uwp); \
    wcl_wb(&(symbol.value), (LP) (new_value))

#define END_SPEC_BIND(symbol) \
    wcl_wb(&(symbol.value), (LP) (uwp.value));	\
    POP_UW_POINT; \
  }

#define BEGIN_UW_PROTECT_BODY \
  { UW_POINT uwp; \
    long flag; \
    uwp.type = UW_PROTECT; \
    if  ((flag = setjmp(uwp.c_env)) == 0) { \
      PUSH_UW_POINT(uwp); \

#define  BEGIN_UW_PROTECT_CLEANUP \
    }

/* HEY! Shouldn't we do the pop before begining the cleanup form? */
#define CONTINUE_FROM_PROTECT \
      POP_UW_POINT; \
      if (flag != 0) unwind(uwp.continue_dest,uwp.value); \
  }

#define THROW(tag,value,mv_holder) throw(tag,value,mv_holder)

#define BEGIN_DYNAMIC_TAG(tag,label) \
    { UW_POINT uwp; int flag; \
      uwp.type = UW_DYNAMIC_TAG; \
      uwp.name = tag;  \
      if  ((flag = setjmp(uwp.c_env)) != 0) \
	goto label; else PUSH_UW_POINT(uwp);
      
#define END_DYNAMIC_TAG \
    POP_UW_POINT; \
    } 

#define GOTO_DYNAMIC_TAG(tag) dynamic_go(tag)

/* These symbol macros are only used by the linker and do not need
   to indirect through the symbol link. */
#define UPDATE_VAR(sym,v,flag_pos) sym.value = (LP) v; \
                                   SET_SYMBOL_FLAG(sym,flag_pos)
	
#define UPDATE_FUNC(sym,value) sym.function = LREF(value)

/* HEY! This is broken, need to put func in hash table */    
#define UPDATE_MACRO(sym,value,flag_pos) UPDATE_FUNC(sym,value); \
                                         SET_SYMBOL_FLAG(sym,flag_pos)
      
extern PROCEDURE ubf_procedure;
extern UBV ubv_marker;
extern LP OE;
LP p_lsp_APPLY(ARGC argc, LP procedure, ...);
LP p_lsp_FUNCALL(ARGC argc, LP procedure, ...);

LP alloc_memory(long len, long word_size, long type);
LP alloc_doubles(long len, long tag);
LP alloc_words(long len, long tag);
LP alloc_shorts(long size, long tag);
LP alloc_bytes(long len, long tag);
LP alloc_bits(long len, long tag);

LP new_closure(LP code, LP env);
LP new_oe(long len);
LP new_float(double n);
LP new_foreign_ptr(LP type, LP ptr);
void lisp_debug();
LP lookup_keyword(LP kwd,LP l);
double float_significand(double f);
double bignum_to_double(LP x);
LP long_to_bignum(long i);
LP ulong_to_bignum(unsigned long i);
long bignum_to_long(LP b);
unsigned long bignum_to_ulong(LP b);
LP c_to_lisp_vector(char* vector,
			   long element_type_tag, long len);
LP c_to_lisp_string(char* string);
LP lisp_to_c_array(LP a);

LP p_lsp_COERCE_2DTO_2DFUNCTION(ARGC argc, LP v_F_0);
LP copy_c_to_lisp_string(char *str);

void wna(ARGC actual, ARGC desired);
void wna_low(ARGC actual, ARGC min);
void wna_high(ARGC actual, ARGC max);
void arg_limit_exceeded(int count);
void init_wcl_threads(LP start_func);
LP wcl_wb(LPL lhs_address, LP rhs);
