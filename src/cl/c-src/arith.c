/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <math.h>
#include <gmp.h>
#include <stdio.h>

#define swap(x,y) {LP tmp = x; x = y; y = tmp;}
#define abs(x) ((x) >= 0 ? (x) : -(x))

extern long integer_add(long i, long j);
extern long integer_subtract(long i, long j);
extern long integer_multiply(long i, long j);
extern LP alloc_float();

mpz_t least_positive_bignum;
mpz_t least_negative_bignum;

/* lots of code duplication here because the wretched C macro
   facility is so weak... */

LP new_float(double f) {
  LP  result;

  result = alloc_float();
  RAW_FLOAT(result) = f;
  return(result);
}

/* HEY! passing floats as unknowns (ala printf) is a general problem...*/
int print_double_float(double f, FILE *stream) {
  int size;
  double frac, i;
  
  /* Hack to get a single 0 after the decimal point sometimes.
     Isn't there some way to get printf to do this by itself???? */
  frac = modf(f,&i);
  if (frac == 0.0) {
    fprintf(stream,"%#.1f%n",f,&size);
  } else {
    fprintf(stream,"%.16G%n",f,&size);
  }
  /* %n seems to be broken under SunOS, so size is probably incorrect */
  return(size);
}

int double_float_to_string(double f, char *buffer) {
  int size;
  double frac, i;
  
  /* Hack to get a single 0 after the decimal point sometimes.
     Isn't there some way to get printf to do this by itself???? */
  frac = modf(f,&i);
  if (frac == 0.0) {
    sprintf(buffer,"%#.1f%n",f,&size);
  } else {
    sprintf(buffer,"%.16G%n",f,&size);
  }
  return(size);
}

double double_round(x, y)
     double x; double y;
{
  double result;

  result = rint(x / y);
}

int double_truncate (double x, double y) {
  int i;
  
  i = (x / y);
  return(i);
}

LP bignum_err(LP x) {
  printf("Finish bignum arith ops!\n");
}

LP ratio_err(LP x) {
  printf("Finish ratio arith ops!\n");
}


LP complex_err(LP x) {
  printf("Finish complex arith ops!\n");
}

LP new_bignum(MP_INT *z) {
  long bits_size = (abs(z->_mp_size) * sizeof(mp_limb_t));
  long total_size = bits_size + sizeof(mpz_t);
  LP result = alloc_bytes(total_size, TYPE_BIGNUM);
  MP_INT *dest = (MP_INT *) (REMOVE_TAG(result));
  dest->_mp_alloc = abs(z->_mp_size);
  dest->_mp_size = z->_mp_size;
  dest->_mp_d = (mp_limb_t *) (dest + 1);
  memcpy(dest + 1, z->_mp_d, bits_size);
  mpz_clear(z);
  return(result);
}

LP normalize_bignum(mpz_t z) {
  long i;

  if ((abs(z->_mp_size) <= 1) &&
      (mpz_cmp(z,least_positive_bignum) < 0) &&
      (mpz_cmp(z,least_negative_bignum) > 0)) {
    i = mpz_get_si(z);
    mpz_clear(z);
    return((LP) LONG_TO_FX(i));
  } else {
    return(new_bignum(z));
  }
}


double bignum_to_double(LP x) {
  return(mpz_get_d(REMOVE_TAG(x)));
}  

LP long_to_bignum(long i) {
    mpz_t z;
    
    mpz_init(z);
    mpz_set_si(z,i);
    return(normalize_bignum(z));
}

LP fixnum_to_bignum(LP fixnum) {
  mpz_t z;
  long i = FX_TO_LONG(fixnum);

  mpz_init(z);
  mpz_set_si(z, i);
  return(new_bignum(z));
}
    
LP bignum_rem(LP x, LP y) {
  mpz_t z;

  mpz_init(z);
  mpz_tdiv_r(z, (MP_INT *) REMOVE_TAG(x), (MP_INT *) REMOVE_TAG(y));
  return(normalize_bignum(z));
}

LP bignum_logand(LP x, LP y) {
    mpz_t z;

  mpz_init(z);
  mpz_and(z, (MP_INT *) REMOVE_TAG(x), (MP_INT *) REMOVE_TAG(y));
  return(normalize_bignum(z));
}

LP bignum_length(LP n) {
  long length;

  length = mpz_sizeinbase(REMOVE_TAG(n), 2);
  return(LONG_TO_FX(length));
}


LP ulong_to_bignum(unsigned long l) {
    mpz_t z;
    
    mpz_init(z);
    mpz_set_ui(z,l);
    return(normalize_bignum(z));
}
     
long bignum_to_long(LP b) {
  long i;

  i = mpz_get_si(REMOVE_TAG(b));
  return(i);
}

unsigned long bignum_to_ulong(LP b) {
  unsigned long l;

  l = mpz_get_si(REMOVE_TAG(b));
  return(l);
}


LP bignum_add(mpz_t x, mpz_t y) {
  mpz_t z;

  mpz_init(z);
  mpz_add(z,x,y);
  return(normalize_bignum(z));
}

LP bignum_subtract(mpz_t x, mpz_t y) {
  mpz_t z;

  mpz_init(z);
  mpz_sub(z, x, y);
  return(normalize_bignum(z));
}

LP bignum_multiply(mpz_t x, mpz_t y) {
  mpz_t z;

  mpz_init(z);
  mpz_mul(z, x, y);
  return(normalize_bignum(z));
}

LP bignum_divide(mpz_t x, mpz_t y) {
  mpz_t z;

  mpz_init(z);
  mpz_tdiv_q(z, x, y);
  return(normalize_bignum(z));
}

     
LP bignum_div(LP x, LP y) {
  bignum_divide((MP_INT *) REMOVE_TAG(x), (MP_INT *) REMOVE_TAG(y));
}


LP addition_overflow_handler(LP x, LP y) {
  mpz_t mpx, mpy, mpz;

  mpz_init(mpx);
  mpz_init(mpy);
  mpz_init(mpz);
  mpz_set_si(mpx, FX_TO_LONG(x));
  mpz_set_si(mpy, FX_TO_LONG(y));
  mpz_add(mpz, mpx, mpy);
  mpz_clear(mpx);
  mpz_clear(mpy);
  return(normalize_bignum(mpz));
}

LP subtraction_overflow_handler(LP x, LP y) {
  mpz_t mpx, mpy, mpz;

  mpz_init(mpx);
  mpz_init(mpy);
  mpz_init(mpz);
  mpz_set_si(mpx, FX_TO_LONG(x));
  mpz_set_si(mpy, FX_TO_LONG(y));
  mpz_sub(mpz, mpx, mpy);
  mpz_clear(mpx);
  mpz_clear(mpy);
  return(normalize_bignum(mpz));
}


LP multiply_overflow_handler(LP x, LP y) {
  mpz_t mpx, mpy, mpz;

  mpz_init(mpx);
  mpz_init(mpy);
  mpz_init(mpz);
  mpz_set_si(mpx, FX_TO_LONG(x));
  mpz_set_si(mpy, FX_TO_LONG(y));
  mpz_mul(mpz, mpx, mpy);
  mpz_clear(mpx);
  mpz_clear(mpy);
  return(normalize_bignum(mpz));
}


LP add(LP x, LP y) {
  mpz_t mpx;
  LP result;

  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      return((LP) integer_add((long) x, (long) y));	
    } else {
    fx_first:
      switch (TAG(y)) {
      case TYPE_FLOAT: return(NEW_FLOAT(FX_TO_LONG(x) + RAW_FLOAT(y))); 
      case TYPE_BIGNUM:
	mpz_init(mpx);
	mpz_set_si(mpx,(FX_TO_LONG(x)));
	result = bignum_add(mpx, (MP_INT *)  REMOVE_TAG(y));
	mpz_clear(mpx);
	return(result);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      swap(x,y); goto fx_first;
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return(NEW_FLOAT(RAW_FLOAT(x) + RAW_FLOAT(y)));
	case TYPE_BIGNUM: float_bignum:
	  return(NEW_FLOAT(RAW_FLOAT(x) + bignum_to_double(y)));
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_bignum;
	case TYPE_BIGNUM: 
	  return(bignum_add((MP_INT *) REMOVE_TAG(x),
			    (MP_INT *) REMOVE_TAG(y)));
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_ratio;
	case TYPE_BIGNUM: swap(x,y); goto bignum_ratio;
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}

LP subtract(LP x, LP y) { 
  mpz_t mpx, mpy;
  LP result;

  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      return((LP) integer_subtract((long) x,(long) y));	
    } else {
      switch (TAG(y)) {
      case TYPE_FLOAT: return(NEW_FLOAT(FX_TO_LONG(x) - RAW_FLOAT(y))); 
      case TYPE_BIGNUM:
	mpz_init(mpx);
	mpz_set_si(mpx,(FX_TO_LONG(x)));
	result = bignum_subtract(mpx, (MP_INT *)  REMOVE_TAG(y));
	mpz_clear(mpx);
	return(result);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      switch (TAG(x)) {
      case TYPE_FLOAT: return(NEW_FLOAT(RAW_FLOAT(x) - FX_TO_LONG(y))); 
      case TYPE_BIGNUM:
	mpz_init(mpy);
	mpz_set_si(mpy,(FX_TO_LONG(y)));
	result = bignum_subtract((MP_INT *) REMOVE_TAG(x), mpy);
	mpz_clear(mpy);
	return(result);
      case TYPE_RATIO: return(ratio_err(x));
      case TYPE_COMPLEX: return(complex_err(x));
      } 
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return(NEW_FLOAT(RAW_FLOAT(x) - RAW_FLOAT(y)));
	case TYPE_BIGNUM: 
	  return(NEW_FLOAT(RAW_FLOAT(x) - bignum_to_double(y)));
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT: 
	  return(NEW_FLOAT(bignum_to_double(x) - RAW_FLOAT(y)));
	case TYPE_BIGNUM:
	  return(bignum_subtract((MP_INT *) REMOVE_TAG(x),
				 (MP_INT *) REMOVE_TAG(y)));
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: return(ratio_err(x));
	case TYPE_BIGNUM: return(ratio_err(x));
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}

LP multiply(LP x, LP y) { 
  mpz_t mpx;
  LP result;

  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      return((LP) integer_multiply((long) x, (long) y));
    } else {
    fx_first:
      switch (TAG(y)) {
      case TYPE_FLOAT: return(NEW_FLOAT(FX_TO_LONG(x) * RAW_FLOAT(y))); 
      case TYPE_BIGNUM:
	mpz_init(mpx);
	mpz_set_si(mpx,(FX_TO_LONG(x)));
	result = bignum_multiply(mpx, (MP_INT *)  REMOVE_TAG(y));
	mpz_clear(mpx);
	return(result);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      swap(x,y); goto fx_first;
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return(NEW_FLOAT(RAW_FLOAT(x) * RAW_FLOAT(y)));
	case TYPE_BIGNUM: float_bignum:
	  return(NEW_FLOAT(RAW_FLOAT(x) * bignum_to_double(y)));
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_bignum;
	case TYPE_BIGNUM:
	  return(bignum_multiply((MP_INT *) REMOVE_TAG(x),
				 (MP_INT *) REMOVE_TAG(y)));
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_ratio;
	case TYPE_BIGNUM: swap(x,y); goto bignum_ratio;
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}

LP divide(LP x, LP y) {
  mpz_t mpx, mpy, mpz;
  int q,r;
  double fx,fy;
  LP result;

  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      r = (long) x % (long) y;
      q = (long) x / (long) y;
      if (r == 0) {
	return((LP) LONG_TO_FX(q)); /* post-adjust */
      } else {
	fx = (long) x;
	fy = (long) y;
	return(NEW_FLOAT(fx / fy));
      }
    } else {
    fx_first:
      switch (TAG(y)) {
      case TYPE_FLOAT: return(NEW_FLOAT(FX_TO_LONG(x) / RAW_FLOAT(y))); 
      case TYPE_BIGNUM:
	mpz_init(mpx);
	mpz_set_si(mpx,(FX_TO_LONG(x)));
	result = bignum_divide(mpx, (MP_INT *) REMOVE_TAG(y));
	mpz_clear(mpx);
	return(result);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      switch (TAG(x)) {
      case TYPE_FLOAT: return(NEW_FLOAT(RAW_FLOAT(x) / FX_TO_LONG(y))); 
      case TYPE_BIGNUM:
	mpz_init(mpy);
	mpz_set_si(mpy, FX_TO_LONG(y));
	result = bignum_divide((MP_INT *) REMOVE_TAG(x), mpy);
	mpz_clear(mpy);
	return(result);
      case TYPE_RATIO: return(ratio_err(x));
      case TYPE_COMPLEX: return(complex_err(x));
      } 
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return(NEW_FLOAT(RAW_FLOAT(x) / RAW_FLOAT(y)));
	case TYPE_BIGNUM: 
	  return(NEW_FLOAT(RAW_FLOAT(x) / bignum_to_double(y)));
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT:
	  return(NEW_FLOAT(bignum_to_double(x) / RAW_FLOAT(y)));
	case TYPE_BIGNUM: 
	  return(bignum_divide((MP_INT *) REMOVE_TAG(x),
			       (MP_INT *) REMOVE_TAG(y)));
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: ratio_err(x);
	case TYPE_BIGNUM: ratio_err(x);
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}

LP num_equal_p(LP x, LP y) {
  mpz_t mpx;
  int flag;

  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      return(((long) x == (long) y) ? T : NIL); 
    } else {
    fx_first:
      switch (TAG(y)) {
      case TYPE_FLOAT: return((FX_TO_LONG(x) == RAW_FLOAT(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(mpx);
	mpz_set_si(mpx, FX_TO_LONG(x));
	flag = mpz_cmp(mpx, (MP_INT *) REMOVE_TAG(y));
	mpz_clear(mpx);
	return((flag == 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      swap(x,y); goto fx_first;
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return((RAW_FLOAT(x) == RAW_FLOAT(y)) ? T : NIL); 
	case TYPE_BIGNUM: float_bignum:
	  return((RAW_FLOAT(x) == bignum_to_double(y)) ? T : NIL);
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_bignum;
	case TYPE_BIGNUM:
	  flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),(MP_INT *) REMOVE_TAG(y));
	  return((flag == 0) ? T : NIL);
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_ratio;
	case TYPE_BIGNUM: swap(x,y); goto bignum_ratio;
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}


LP greaterp(LP x, LP y) {
  mpz_t bignum;
  int flag;
 
  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      return(((long) x > (long) y) ? T : NIL); 
    } else {
    fx_first:
      switch (TAG(y)) {
      case TYPE_FLOAT: return((FX_TO_LONG(x) > RAW_FLOAT(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(bignum);
	mpz_set_si(bignum, FX_TO_LONG(x));
	flag = mpz_cmp(bignum,(MP_INT *) REMOVE_TAG(y));
	mpz_clear(bignum);
	return((flag > 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      switch (TAG(x)) {
      case TYPE_FLOAT: return((RAW_FLOAT(x) > FX_TO_LONG(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(bignum);
	mpz_set_si(bignum, FX_TO_LONG(y));
	flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),bignum);
	mpz_clear(bignum);
	return((flag > 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(x));
      case TYPE_COMPLEX: return(complex_err(x));
      } 
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return((RAW_FLOAT(x) > RAW_FLOAT(y)) ? T : NIL); 
	case TYPE_BIGNUM: 
	  return((RAW_FLOAT(x) > bignum_to_double(y)) ? T : NIL);
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT:
	  return((bignum_to_double(x) > RAW_FLOAT(y)) ? T : NIL);
	case TYPE_BIGNUM: 
	  flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),(MP_INT *) REMOVE_TAG(y));
	  return((flag > 0) ? T : NIL);
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_ratio;
	case TYPE_BIGNUM: swap(x,y); goto bignum_ratio;
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}

LP geq_p(LP x, LP y) {
  mpz_t bignum;
  int flag;
 
  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      return(((long) x >= (long) y) ? T : NIL); 
    } else {
    fx_first:
      switch (TAG(y)) {
      case TYPE_FLOAT: return((FX_TO_LONG(x) >= RAW_FLOAT(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(bignum);
	mpz_set_si(bignum, FX_TO_LONG(x));
	flag = mpz_cmp(bignum,(MP_INT *) REMOVE_TAG(y));
	mpz_clear(bignum);
	return((flag >= 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      switch (TAG(x)) {
      case TYPE_FLOAT: return((RAW_FLOAT(x) >= FX_TO_LONG(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(bignum);
	mpz_set_si(bignum, FX_TO_LONG(y));
	flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),bignum);
	mpz_clear(bignum);
	return((flag >= 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(x));
      case TYPE_COMPLEX: return(complex_err(x));
      } 
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return((RAW_FLOAT(x) >= RAW_FLOAT(y)) ? T : NIL); 
	case TYPE_BIGNUM: 
	  return((RAW_FLOAT(x) >= bignum_to_double(y)) ? T : NIL);
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT:
	  return((bignum_to_double(x) >= RAW_FLOAT(y)) ? T : NIL);
	case TYPE_BIGNUM: 
	  flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),(MP_INT *) REMOVE_TAG(y));
	  return((flag >= 0) ? T : NIL);
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_ratio;
	case TYPE_BIGNUM: swap(x,y); goto bignum_ratio;
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}

LP lessp(LP x, LP y) {
  mpz_t bignum;
  int flag;

  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      return(((long) x < (long) y) ? T : NIL); 
    } else {
    fx_first:
      switch (TAG(y)) {
      case TYPE_FLOAT: return((FX_TO_LONG(x) < RAW_FLOAT(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(bignum);
	mpz_set_si(bignum, FX_TO_LONG(x));
	flag = mpz_cmp(bignum,(MP_INT *) REMOVE_TAG(y));
	mpz_clear(bignum);
	return((flag < 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      switch (TAG(x)) {
      case TYPE_FLOAT: return((RAW_FLOAT(x) < FX_TO_LONG(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(bignum);
	mpz_set_si(bignum, FX_TO_LONG(y));
	flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),bignum);
	mpz_clear(bignum);
	return((flag < 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(x));
      case TYPE_COMPLEX: return(complex_err(x));
      } 
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return((RAW_FLOAT(x) < RAW_FLOAT(y)) ? T : NIL); 
	case TYPE_BIGNUM: 
	  return((RAW_FLOAT(x) < bignum_to_double(y)) ? T : NIL);
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT: 
	  return((bignum_to_double(x) < RAW_FLOAT(y)) ? T : NIL);
	case TYPE_BIGNUM: 
	  flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),(MP_INT *) REMOVE_TAG(y));
	  return((flag < 0) ? T : NIL);
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_ratio;
	case TYPE_BIGNUM: swap(x,y); goto bignum_ratio;
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}

LP leq_p(LP x, LP y) {
  mpz_t bignum;
  int flag;
 
  if FIXNUMP(x) { 
    if FIXNUMP(y) {
      return(((long) x <= (long) y) ? T : NIL); 
    } else {
    fx_first:
      switch (TAG(y)) {
      case TYPE_FLOAT: return((FX_TO_LONG(x) <= RAW_FLOAT(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(bignum);	
	mpz_set_si(bignum, FX_TO_LONG(x));
	flag = mpz_cmp(bignum,(MP_INT *) REMOVE_TAG(y));
	mpz_clear(bignum);
	return((flag <= 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(y));
      case TYPE_COMPLEX: return(complex_err(y));
      } 
    } 
  } else {
    if FIXNUMP(y) {
      switch (TAG(x)) {
      case TYPE_FLOAT: return((RAW_FLOAT(x) <= FX_TO_LONG(y)) ? T : NIL); 
      case TYPE_BIGNUM:
	mpz_init(bignum);
	mpz_set_si(bignum, FX_TO_LONG(y));
	flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),bignum);
	mpz_clear(bignum);
	return((flag <= 0) ? T : NIL);
      case TYPE_RATIO: return(ratio_err(x));
      case TYPE_COMPLEX: return(complex_err(x));
      } 
    }  else { 
      switch (TAG(x)) {
      case TYPE_FLOAT:
	switch (TAG(y)) {
	case TYPE_FLOAT: return((RAW_FLOAT(x) <= RAW_FLOAT(y)) ? T : NIL); 
	case TYPE_BIGNUM:
	  return((RAW_FLOAT(x) <= bignum_to_double(y)) ? T : NIL);
	case TYPE_RATIO: float_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: float_complex: return(complex_err(y));
	}
      case TYPE_BIGNUM:
	switch (TAG(y)) {
	case TYPE_FLOAT: 
	  return((bignum_to_double(x) <= RAW_FLOAT(y)) ? T : NIL);
	case TYPE_BIGNUM:
	  flag = mpz_cmp((MP_INT *) REMOVE_TAG(x),(MP_INT *) REMOVE_TAG(y));
	  return((flag <= 0) ? T : NIL);
	case TYPE_RATIO: bignum_ratio: return(ratio_err(y));
	case TYPE_COMPLEX: bignum_complex: return(complex_err(y));
	}
      case TYPE_RATIO:
	switch (TAG(y)) {
	case TYPE_FLOAT: swap(x,y); goto float_ratio;
	case TYPE_BIGNUM: swap(x,y); goto bignum_ratio;
	case TYPE_RATIO: return(ratio_err(y));
	case TYPE_COMPLEX: ratio_complex: return(complex_err(y));
	}
      case TYPE_COMPLEX:
	swap(x,y);
	switch (TAG(x)) {
	case TYPE_FLOAT: goto float_complex;
	case TYPE_BIGNUM: goto bignum_complex;
	case TYPE_RATIO: goto ratio_complex;
	case TYPE_COMPLEX: return(complex_err(x));
	}
      }
    } 
  } 
  p_lsp_ARITH_2DERROR(2,x,y);
}

LP c_eql(LP x, LP y) {
  if (x == y) {			/* works for chars  + fixnums */
    return(T);
  } else {
    if (OTHER_PTRP(x) && OTHER_PTRP(y) &&
	((TAG(x) & 3) == 1) && ((TAG(y) & 3) == 1)) {
      return(num_equal_p(x,y));
    }
  }
  return(NIL);
}

double float_significand(double f) {
  int exponent;
  
  return(frexp(f,&exponent));
}

int float_exponent(double f) {
  int exponent;
  
  frexp(f,&exponent);
  return(exponent);
}

int int_length(long n) {
  int len; 
  for (len = 0; n > 0; len = len + 1) {
    n = n >> 1;
  }
  return(len);
}

void init_arith() {
  mpz_init(least_positive_bignum);
  mpz_init(least_negative_bignum);
  mpz_set_si(least_positive_bignum, (MOST_POSITIVE_FIXNUM + 1));
  mpz_set_si(least_negative_bignum, (MOST_NEGATIVE_FIXNUM - 1));
}

