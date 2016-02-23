/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <stdio.h>
#include <string.h>

LP p_lsp_ILLEGAL_2DELEMENT_2DTYPE(int argc, LP a, LP value);
LP p_lsp_MULTI_2DARRAY_2DBOUNDS_2DERROR(int argc, LP a, LP indices);
LP p_lsp_VECTOR_2DBOUNDS_2DERROR(int argc, LP v, LP fx_i);
LP p_lsp_NOT_2DARRAY_2DERROR(int argc, LP a, LP x);

#define VECTOR_BOUNDS_CHECK(v,fx_i,i)			   \
  if (!(FIXNUMP(fx_i) && (i >= 0) && (i < LEN_FIELD(v))))  \
  p_lsp_VECTOR_2DBOUNDS_2DERROR(2,v,fx_i);    

LP simple_vref_1(LP a, long i, int tag) {
  long w;

  switch (tag & ARRAY_ELEMENT_TYPE_MASK) {
  case TYPE_SIMPLE_BIT_VECTOR:
    w = *((unsigned long *) (a - 1) + (i / (sizeof(long) * 8)));
    return((LP) LONG_TO_FX((w >> (i % (sizeof(long) * 8))) & 1));
  case TYPE_SIMPLE_SIGNED_8BIT_VECTOR:
    return((LP) LONG_TO_FX(*((signed char *) (a - 1) + i)));
  case TYPE_SIMPLE_UNSIGNED_8BIT_VECTOR:
    return((LP) LONG_TO_FX(*((unsigned char *) (a - 1) + i)));
  case TYPE_SIMPLE_STRING:
    return(NEW_CHAR(*(a - 1 + i)));
  case TYPE_SIMPLE_SIGNED_16BIT_VECTOR:
    return((LP) LONG_TO_FX(*((signed short *) (a - 1) + i)));
  case TYPE_SIMPLE_UNSIGNED_16BIT_VECTOR:
    return((LP) LONG_TO_FX(*((unsigned short *) (a - 1) + i)));
  case TYPE_SIMPLE_SIGNED_32BIT_VECTOR:
    return((LP) LONG_TO_INTEGER(*((signed long *) (a - 1) + i)));
  case TYPE_SIMPLE_UNSIGNED_32BIT_VECTOR:
    return((LP) ULONG_TO_INTEGER(*((unsigned long *) (a - 1) + i)));
  case TYPE_SIMPLE_VECTOR:
    return((LP) *((unsigned long *) (a - 1) + i));
  case TYPE_SIMPLE_FLOAT_VECTOR:
    return(NEW_FLOAT(*((double *) (a - 1) + i)));
  default:
    printf("Internal error: Illegal array element type\n");
  }
}

LP set_simple_vref_1(LP value, LP a, long i, int tag) {
  long w;
  long  word_index, mask;

  switch (TAG(a) & ARRAY_ELEMENT_TYPE_MASK) {
  case TYPE_SIMPLE_BIT_VECTOR:
    word_index = (i / (sizeof(long) * 8));
    w = *((unsigned long *) (a - 1) + word_index);
    mask = (1L << (i % (sizeof(long) * 8)));
    if (value == 0) {
      w = (~mask) & w;
    } else {
      w = mask | w;
    }
    *((unsigned long *) (a - 1) + word_index) = w;
    break;
  case TYPE_SIMPLE_SIGNED_8BIT_VECTOR:
    *((signed char *) a - 1 + i) = FX_TO_LONG(value);
    break;
  case TYPE_SIMPLE_UNSIGNED_8BIT_VECTOR:
    *((unsigned char *) a - 1 + i) = FX_TO_LONG(value);
    break;
  case TYPE_SIMPLE_STRING:
    *(a - 1 + i) = DEREF(value);
    break;
  case TYPE_SIMPLE_SIGNED_16BIT_VECTOR:
    *((signed short *) (a - 1) + i) = FX_TO_LONG(value);
    break;
  case TYPE_SIMPLE_UNSIGNED_16BIT_VECTOR:
    *((unsigned short *) (a - 1) + i) = FX_TO_LONG(value);
    break;
  case TYPE_SIMPLE_SIGNED_32BIT_VECTOR:
    *((signed long *) (a - 1) + i) = INTEGER_TO_LONG(value);
    break;
  case TYPE_SIMPLE_UNSIGNED_32BIT_VECTOR:
    *((unsigned long *) (a - 1) + i) = INTEGER_TO_ULONG(value);
    break;
  case TYPE_SIMPLE_VECTOR:
    *((unsigned long *) (a - 1) + i) = (unsigned long) value;
    break;
  case TYPE_SIMPLE_FLOAT_VECTOR:
    *((double *) (a - 1) + i) = RAW_FLOAT(value);
    break;
  default:
    p_lsp_ILLEGAL_2DELEMENT_2DTYPE(2,a,value);
  }
  return(value);
}

int multi_to_vector_index(LP a, LP indices) {
  long index,rank,i,element_size,vector_index;
  LP mult;
  LP dims;

  dims = (LP) DEREF(a + sizeof(long));
  mult = (LP) DEREF(a + (sizeof(long) * 2));
  rank = LEN_FIELD(dims);
  vector_index = 0;

  for (i = 0; i < rank; i++) {
    index = FX_TO_LONG(DEREF(indices + i * sizeof(long))); 
    if ((index >= 0) && (index < DEREF(dims + i * sizeof(long)))) {
      vector_index = vector_index + (index * DEREF(mult + i * sizeof(long)));
    } else {
      p_lsp_MULTI_2DARRAY_2DBOUNDS_2DERROR(2,a,indices);
    }
  }
  return(vector_index);
}

LP simple_vref(LP a, LP index) {
  long i = FX_TO_LONG(index);
  VECTOR_BOUNDS_CHECK(a,index,i)
  return(simple_vref_1(a,i,TAG(a)));
}


LP set_simple_vref(LP value, LP a, LP index) {
  long i = FX_TO_LONG(index);
  VECTOR_BOUNDS_CHECK(a,index,i);
  return(set_simple_vref_1(value,a,i,TAG(a)));
}

LP simple_aref(LP a, LP indices) {
  int index = multi_to_vector_index(a,indices);
  return(simple_vref((LP) DEREF(a),(LP) LONG_TO_FX(index)));
}

LP set_simple_aref(LP value, LP a, LP indices) {
  int index = multi_to_vector_index(a,indices);
  return(set_simple_vref(value,(LP) DEREF(a),(LP) LONG_TO_FX(index)));
}

LP complex_vref(LP a, LP index) {
  long i = FX_TO_LONG(index);
  VECTOR_BOUNDS_CHECK(a,index,i)
  i = i + FX_TO_LONG(LDREF(a,COMPLEX_VECTOR,displaced_index_offset));
  return(simple_vref_1((LP) DEREF(a),i,TAG(a)));
}

LP set_complex_vref(LP value, LP a, LP index) {
  long i = FX_TO_LONG(index);
  VECTOR_BOUNDS_CHECK(a,index,i);
  i = i + FX_TO_LONG(LDREF(a,COMPLEX_VECTOR,displaced_index_offset));
  return(set_simple_vref_1(value,(LP) DEREF(a),i,TAG(a)));
}

/* HEY! fix these to pass tag of outer array and add displacement */
LP complex_aref(LP a, LP indices) {
  return(simple_aref((LP) DEREF(a),indices));
}

LP set_complex_aref(LP value, LP a, LP indices) {
  return(set_simple_aref(value,(LP) DEREF(a),indices));
}


LP c_aref(LP a, LP indices) {
  if (OTHER_PTRP(a)) {
    switch (TAG(a) & 0x7) {
    case SIMPLE_VECTOR_TAG: return(simple_vref(a,(LP) DEREF(indices)));
    case SIMPLE_MULTI_ARRAY_TAG: return(simple_aref(a,indices));
    case COMPLEX_VECTOR_TAG: return(complex_vref(a,(LP) DEREF(indices)));
    case COMPLEX_MULTI_ARRAY_TAG: return(complex_aref(a,indices));
    }
  }
  p_lsp_NOT_2DARRAY_2DERROR(2,a,NIL);
}


LP c_set_aref(LP value, LP a, LP indices) {
  if (OTHER_PTRP(a)) {
    switch (TAG(a) & 0x7) {
    case SIMPLE_VECTOR_TAG:
      return(set_simple_vref(value,a,(LP) DEREF(indices)));
    case SIMPLE_MULTI_ARRAY_TAG: return(set_simple_aref(value,a,indices));
    case COMPLEX_VECTOR_TAG:
      return(set_complex_vref(value,a,(LP) DEREF(indices)));
    case COMPLEX_MULTI_ARRAY_TAG: return(set_complex_aref(value,a,indices));
    }
  }
  p_lsp_NOT_2DARRAY_2DERROR(2,a,NIL);
}


LP vref(LP a, LP i) {
  if (OTHER_PTRP(a)) {
    switch (TAG(a) & 0x7) {
    case SIMPLE_VECTOR_TAG: return(simple_vref(a,i));
    case COMPLEX_VECTOR_TAG: return(complex_vref(a,i));
    }
  }
  p_lsp_NOT_2DARRAY_2DERROR(2,a,T);
}


LP set_vref(LP value, LP a, LP i) {
  if (OTHER_PTRP(a)) {
    switch (TAG(a) & 0x7) {
    case SIMPLE_VECTOR_TAG: return(set_simple_vref(value,a,i));
    case COMPLEX_VECTOR_TAG: return(set_complex_vref(value,a,i));
    }
  }
  p_lsp_NOT_2DARRAY_2DERROR(2,a,T);
}

LP initialize_array(LP array,
		    int element_type,
		    int element_size,
		    LP initial_element) {
  int len = (HEADER(array) >> 8) - 1;

  array = array - 1;
  switch (element_size) {
  case 64:
    {  double *ptr;
       double init = RAW_FLOAT(initial_element);
       
       ptr = (double *) array;
       while (len >= 0) {
	 *(ptr + len--) = init;
       }
     }
    break;

  case 32:
    {  unsigned long *ptr;
       unsigned long init;

       if (element_type == ELEMENT_TYPE_PTR) {
	 init = (unsigned long) initial_element;
       } else {
	 init = FX_TO_LONG(initial_element);
       }

       ptr = (unsigned long *) array;
       while (len >= 0) {
	 *(ptr + len--) =  init;
       }
     }
    break;

  case 16:
    {  unsigned short *ptr;
       unsigned char init = FX_TO_LONG(initial_element);
       
       ptr = (unsigned short *) array;
       while (len >= 0) {
	 *(ptr + len--) = init;
       }
     }
    break;

  case 8:
    {  unsigned char *ptr;
       unsigned char init;

       if (element_type == ELEMENT_TYPE_CHAR) {
	 init = RAW_CHAR(initial_element);
       } else {
	 init = FX_TO_LONG(initial_element);
       }
       ptr = (unsigned char *) array;
       while (len >= 0) {
	 *(ptr + len--) = init;
       }
     }
    break;

  case 1:
    break;
  }
  return(array + 1);
}

LP c_to_lisp_vector(char *vector,long element_type_tag,long len) {
  LP v;
  v = alloc_bytes(sizeof(COMPLEX_VECTOR) - sizeof(long),TYPE_SIMPLE_VECTOR);
  LHEADER(v) = (len << 8) + (element_type_tag + COMPLEX_VECTOR_TAG);
  /* It's ok to put a pointer outside the heap here because the GC
     will ignore it. */
  LDREF(v,COMPLEX_VECTOR,underlying_vector) = ADD_TAG(vector);
  return(v);
}

/* This creates a complex vector that points at a c string */
LP c_to_lisp_string(char *string) {
  if (string == NULL) {
    return(NIL);
  } else {
    return(c_to_lisp_vector(string,ELEMENT_TYPE_CHAR,strlen(string)));
  }
}

/* This creates copies a c string into a new lisp simple string */
LP copy_c_to_lisp_string(char *str) {
   LP ptr;
   int i,len;

   len = strlen(str) + 1;
   ptr = alloc_bytes(len,TYPE_SIMPLE_STRING);
   for (i = 0; i < len; i++) {
     *(REMOVE_TAG(ptr + i)) = *(str + i);
   }
   LHEADER(ptr) = ((len - 1) << 8) + TAG(ptr);
   return(ptr);
}


LP lisp_to_c_array(LP a) {
  int tag;
  
  if (OTHER_PTRP(a)) {
    switch (TAG(a) & 0x5) {
     case 0: return(a - 1);
     case 4: return(REMOVE_TAG(LDREF(a,COMPLEX_VECTOR,underlying_vector)));
     }
  }
  if (a == 0) {
    return(0);			/* hack to pass null ptr in from lisp  */
  }
  printf("lisp_to_c_array: not an array\n");
  lisp_debug();
}


