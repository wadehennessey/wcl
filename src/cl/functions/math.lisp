;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun abs (n)
  (if (plusp n)
      n
      (- n)))

(defun-inline acos (x)
  (double_acos (float x)))

(defun arith-error (x y)
  (error "~S is not a number" (if (numberp x) y x)))

;;; HEY! write a version that uses %ash[l,r] and detects overflow?
(defmethod ash ((n integer) (count fixnum))
  (if (plusp count)
      (* n (%ashl 1 count))
      (values (truncate n (%ashl 1 (abs count))))))

(defun-inline asin (x)
  (double_asin (float x)))

(defun atan/2 (y x)			; HEY! fix this...
  (double_atan2 (float y) (float x)))

(defun atan (y &optional (x 0))
  (atan/2 y x))

(defun bignum-div (x y)
  (bignum_div (ensure-bignum x) (ensure-bignum y)))

(defun bignum-rem (x y)
  (bignum_rem (ensure-bignum x) (ensure-bignum y)))

(defun boole (op i1 i2)
  (select op
    (boole-clr 0)
    (boole-set 1)
    (boole-1 i1)
    (boole-2 i2)
    (boole-c1 (lognot i1))
    (boole-c2 (lognot i2))
    (boole-and (logand i1 i2))
    (boole-ior (logior i1 i2))
    (boole-xor (logxor i1 i2))
    (boole-eqv (logeqv i1 i2))
    (boole-nand (lognand i1 i2))
    (boole-nor (lognor i1 i2))
    (boole-andc1 (logandc1 i1 i2))
    (boole-andc2 (logandc2 i1 i2))
    (boole-orc1 (logorc1 i1 i2))
    (boole-orc2  (logorc2 i1 i2))))

(defun write-byte-specifier (byte-spec stream depth)
  (format stream "(BYTE ~D ~D)"
	  (byte-size byte-spec) (byte-position byte-spec)))

(defun-inline byte (size position)
  (make-byte :size size :position position))

(defmethod ldb ((byte-spec byte-specifier) (n integer))
  (ldb-1 (byte-size byte-spec) (byte-position byte-spec) n))

(defun ldb-1 (size position n)
  (declare (fixnum size position))
  (etypecase n
    (fixnum (ldb/fixnum size position n))
    (bignum (ldb/bignum size position n))))

(defun-inline ldb/fixnum (size position n)
 (%load-field (%field-mask size) position n))

(defun ldb/bignum (size position n)
  (logandc2 (ash n (- position))
            (- (ash 1 size))))

(defmethod dpb ((value integer) (byte-spec byte-specifier)  (n integer))
  (dpb-1 value (byte-size byte-spec) (byte-position byte-spec) n))

(defun dpb-1 (value size position n)
  (etypecase n
    (fixnum (dpb/fixnum value size position n))
    (bignum (dpb/bignum value size position n))))

(defun dpb/fixnum (value size position n)
  ;; HEY! write %store-field and use it here!
  (dpb/bignum value size position n))

(defun dpb/bignum (value size position n)
  (logxor n
          (mask-field-1 size position n)
          (ash (logandc2 value
                         (- (ash 1 size)))
	       position)))

(defun ldb-test (byte-spec n)
  (not (= (ldb byte-spec n) 0)))

(defun mask-field (byte-spec n)
  (mask-field-1 (byte-size byte-spec) (byte-position byte-spec)))

(defun mask-field-1 (size position n)
  (ash (ldb-1 size position n) position))

(defun deposit-field (value byte-spec n)
  (dpb (ash value (- (byte-position byte-spec))) byte-spec n))

(defun-inline ceiling/fixnum (x y)
  (let ((q (%div x y))
	(r (%rem x y)))
    (values (if (= 0 r) q (1+ q)) (- r y))))

(defun-inline ceiling/float (x y)
  (values (double_truncate (ceil (/ x y)) 1.0)
	  (- (fmod x y) y)))

(defun ceiling (x &optional (y 1))
  (if (and (fixnump x) (fixnump y))
      (ceiling/fixnum x y)
      (ceiling/float (float x) (float y))))

(defun-inline complex (realpart &optional (imagpart 0))
  (make-complex realpart imagpart))

(defun make-complex (realpart imagpart)
  (let ((c (alloc_words 2 type-complex)))
    (%32bit-def c 0 realpart)
    (%32bit-def c 1 imagpart)
    c))

(defun-inline cos (x)
  (double_cos (float x)))

(defmethod decode-float ((x float))
  (values (float_significand x)  
	  (float_exponent x)
	  (if (< x 0) -1.0 1.0)))

(defmethod integer-decode-float ((x float))
  (error "write integer-decode-float!"))

(defun / (x &restv others)
  (let ((n (length others)))
    (case n
      (0 x)
      (1 (divide x (svref others 0)))
      (t (divide x (accumulate-lr divide n others))))))

(defun ensure-bignum (x)
  (etypecase x
    (fixnum (fixnum_to_bignum x))
    (bignum x)))

(defun ensure-fixnum (n)
  (etypecase n
    (fixnum n)))

(defun evenp (integer)
  (zerop (rem integer 2)))

(defun-inline exp (n)
  (double_exp (float n)))

(defun expt (n power)
  (if (and (fixnump n) (fixnump power))
      (expt/fixnum n power)
      (expt/float n power)))

(defun expt/fixnum (n power)
  (if (>= power 0)
      (loop for i from power downto 0
	    as result = 1 then (* result n)
	    finally (return result))
      (loop for i from power upto 0
	    as result = 1 then (/ result n)
	    finally (return result))))

(defun expt/float (n power)
  (pow (float n) (float power)))

(defun ffloor (x &optional (y 1.0))
  (let ((float-x (float x))
	(float-y (float y)))
    (values (double_floor (/ float-x float-y))
	    (fmod float-x float-y))))

(defmethod float-radix ((x float)) 2)

(defun-inline float (n &optional other)
  (float-1 n))

(defun float-1 (n)
  (typecase n
    (float n)
    (fixnum (%int->double n))
    (bignum (bignum_to_double n))
    (ratio (/ (float (numerator n)) (float (denominator n))))
    (t  (error "~A is not a number" n))))

(defun-inline floor/fixnum (x y)
  (values (%div x y) (%rem x y)))

(defun-inline floor/float (x y)
  (values (double_truncate (double_floor (/ x y)) 1.0)
	  (fmod x y)))

(defun floor (x &optional (y 1))
  (if (and (fixnump x) (fixnump y))
      (floor/fixnum x y)
      (floor/float (float x) (float y))))

(defun floor/1v (x &optional (y 1))
  (floor x y))

(defun gcd/2 (x y)
  (let ((rem (rem x y)))
    (if (= rem 0)
	y
	(gcd/2 y rem))))

(defun gcd (&restv args)
  (let ((n (length args)))
    (case n
      (0 0)
      (1 (first args))
      (t (accumulate-lr gcd/2 n args)))))

(def-nary-pred >= geq_p)

(def-nary-pred > greaterp)

(defmethod imagpart ((c complex))
  (%32bit-ref c 1))

;;; Computes: (ceiling (log (if (< n 0) (- n) (1+ n)) 2))
(defmethod integer-length ((n integer))
  (let ((x (if (< n 0) (- (+ n 1)) n)))
    (etypecase x
      (fixnum (int_length x))
      (bignum (bignum_length x)))))

(defun lcm/2 (n m)
  (* (/ (max n m) (gcd n m)) (min n m)))

(defun lcm (&restv args)
  (let ((n (length args)))
    (case n
      (0 1)
      (1 (first args))
      (t (accumulate-lr lcm/2 n args)))))

(def-nary-pred <= leq_p)

(def-nary-pred < lessp)

(defun-inline log/10 (n)
  (double_log10 (float n)))

(defun log/e (n)
  (double_log (float n)))

(defun log (n &optional base)
  (let ((f (float n)))
    (case base
      (nil (log/e f))
      (t (/ (log/e f) (log/e (float base)))))))

;;; HEY! Put C version in arith.c?
(defmethod logand/2 ((x integer) (y integer))
  (if (and (fixnump x) (fixnump y))
      (%logand x y)
      (bignum_logand (ensure-bignum x) (ensure-bignum y))))

(def-nary logand logand/2)

(defun logandc1 (i1 i2)
  (logand (lognot i1) i2))

(defun logandc2 (i1 i2)
  (logand i1 (lognot i2)))

(defmethod logeqv/2 ((x fixnum) (y fixnum))
  (error "write LOGEQV"))

;;; HEY! Should accept 0 args too
(def-nary logeqv logeqv/2)

(defmethod-inline logior/2 ((x fixnum) (y fixnum))
  (%logior x y))

(def-nary logior logior/2)

(defun lognand (i1 i2)
  (lognot (logand i1 i2)))

(defun-inline lognor (i1 i2)
  (lognot (logior i1 i2)))

(defmethod-inline lognot ((x fixnum))
  (%lognot x))

(defmethod logbitp ((index fixnum) (n integer))
  (if (and (fixnump n) (< index 32))
      (%logbitp index n)      
      (ldb-test (byte 1 index) n)))

(defun logtest (x y)
  (not (= (logand x y) 0)))

(defun-inline logorc1 (i1 i2)
  (logior (lognot i1) i2))

(defun-inline logorc2 (i1 i2)
  (logior i1 (lognot i2)))

(defmethod-inline logxor/2 ((x fixnum) (y fixnum))
  (%logxor x y))

(def-nary logxor logxor/2)

(defun make-ratio (numerator denominator)
  (let* ((gcd (gcd/2 numerator denominator))
	 (n (/ numerator gcd))
	 (d (/ denominator gcd)))
    (cond ((= d 1) n)
	  ((= n 0) 0)
	  ((= d 0) (error "Cannot divide by 0: ~A/~A" n d))
	  (t (let ((r (alloc_words 2 type-ratio)))
	       (%32bit-def r 0 n)
	       (%32bit-def r 1 d)
	       r)))))

(defun-inline max/2 (x y)
  (if (> x y) x y))

(def-nary max max/2)

(defun-inline min/2 (x y)
  (if (< x y) x y))

(def-nary min min/2)

(defun - (&restv args)
  (let ((n (length args)))
    (case n
      (0 0)
      (1 (subtract 0 (first args)))
      (t (accumulate-lr subtract n args)))))

(defun-inline minusp (n)
  (< n 0))

(defun mod (x y)
  (if (and (fixnump x) (fixnump y))
      (%rem x y)
      (bignum_rem (ensure-bignum x) (ensure-bignum y))))

(def-nary-pred = num_equal_p)

(defun num-not-equal-p (n1 n2)
  (not (num_equal_p n1 n2)))

(def-nary-pred /= num-not-equal-p)

(defmethod numerator ((r ratio))
  (%32bit-ref r 0))

(defmethod denominator ((r ratio))
  (%32bit-ref r 1))

(defun oddp (integer)
  (not (evenp integer)))

(defun-inline 1- (x)
  (- x 1))

(defun-inline 1+ (x)
  (+ x 1))

(defun + (&restv args)
  (let ((n (length args)))
    (case n
      (0 0)
      (1 (first args))
      (t (accumulate-lr add n args)))))

(defun-inline plusp (n)
  (> n 0))

(defun rationalize (n)
  (rational n))

(defun rational (n)
  (typecase n
    (rational n)
    (float (rational/float n))
    (t  (error "~A should be a rational or floating point number" n))))

(defun rational/float (f)
  (multiple-value-bind (whole fraction)
      ;; HEY! use ffloor
      (truncate f)
    (warn "rational/float: FIX ME! ~A converts to ~A" f whole)
    whole))

(defmethod realpart ((c complex))
  (%32bit-ref c 0))

(defmethod rem (x y)
  (if (and (fixnump x) (fixnump y))
      (%rem x y)
      (bignum_rem (ensure-bignum x) (ensure-bignum y))))

(defun round/2 (number divisor)
  (multiple-value-bind (tru rem) (truncate number divisor)
    (let ((thresh (/ (abs divisor) 2)))
      (cond ((or (> rem thresh)
		 (and (= rem thresh) (oddp tru)))
	     (if (minusp divisor)
		 (values (- tru 1) (+ rem divisor))
		 (values (+ tru 1) (- rem divisor))))
	    ((let ((-thresh (- thresh)))
	       (or (< rem -thresh)
		   (and (= rem -thresh) (oddp tru))))
	     (if (minusp divisor)
		 (values (+ tru 1) (- rem divisor))
		 (values (- tru 1) (+ rem divisor))))
	    (t (values tru rem))))))

(defun-inline round (n &optional (divisor 1))
  (round/2 n divisor))

(defmethod scale-float ((x float) (exponent fixnum))
  (ldexp x exponent))

(defun signum (n)
  (if (= n 0)
      n
      (if (rationalp n)
	  (if (plusp n) 1 -1)
	  (/ n (abs n)))))

(defun-inline sin (x)
  (double_sin (float x)))

(defun sqrt (x)				; CL says ok to always return a float
  (if (minusp x)
      (complex 0 (double_sqrt (float (- x))))
      (double_sqrt (float x))))

(defun tan (x)
  (double_tan (float x)))

(defun * (x &restv others)
  (let ((n (length others)))
    (case n
      (0 x)
      (1 (multiply x (svref others 0)))
      (t (multiply x (accumulate-lr multiply n others))))))

(defun-inline truncate (n &optional (divisor 1))
  (truncate/2 n divisor))

(defun truncate/2 (n divisor)
  (etypecase n
    (fixnum (etypecase divisor
	      (fixnum (values (%div n divisor) (%rem n divisor)))
	      ;; HEY! add bignum case.
	      (float (truncate-float/2 (float n) divisor))
	      ;; HEY! ratio case is a hack for now.
	      (ratio (truncate-float/2 (float n) (float divisor)))))
    (bignum (etypecase divisor
	      (integer (values (bignum-div n divisor) (bignum-rem n divisor)))
	      (float (truncate-float/2 (float n) divisor))))
    (float (truncate-float/2 n (float divisor)))))

(defun truncate-float/2 (n divisor)
  (values (double_truncate n divisor)
	  (fmod n divisor)))

(defun-inline zerop (n)
  (= n 0))

