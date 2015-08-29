;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(deftype cons ()
  '(satisfies consp))

(deftype foreign-pointer ()
  '(satisfies foreign-pointer-p))

(deftype vector (&optional type length)
  `(array ,type (,length)))

(deftype simple-vector (&optional length)
  `(simple-array t (,length)))

(deftype simple-string (&optional length)
  `(simple-array character (,length)))

(deftype string (&optional length)
  `(array character (,length)))

;;; HEY! What is this really? (array (unsigned-byte 32) (*)) ????
;;; give predicate a better name
(deftype 32bit-vector ()
  '(satisfies 32bit-vector-p))

(deftype bit-array (&optional dims)
  `(array bit ,dims))

(deftype bit-vector (&optional length)
  `(vector bit length))

(deftype complex-array ()
  '(satisfies complex-array-p))

(deftype vector-with-fill-pointer ()
  '(satisfies vector-with-fill-pointer-p))

(deftype sequence ()
  '(or list vector))

(deftype list ()
  '(or null cons))

(deftype fixnum ()
  `(integer ,most-negative-fixnum ,most-positive-fixnum))

(deftype bignum ()
  '(satisfies bignump))

(deftype ratio ()
  '(satisfies ratiop))

(deftype complex ()
  '(satisfies complexp))

(deftype real () '(or rational float))

(deftype number ()
  '(or integer float ratio complex))	

;;; HEY! fix this now that we have ratios...
(deftype rational (&optional low high)
  `(integer ,low ,high))

(deftype null ()
  '(satisfies null))

(deftype atom ()
  '(satisfies atom))

(deftype function-specifier ()
  '(or symbol cons))

(deftype function ()
  '(satisfies functionp))

(deftype compiled-function ()
  '(satisfies compiled-function-p))

(deftype procedure ()
  '(satisfies procedurep))

(deftype keyword ()
  '(satisfies keywordp))

(deftype string-arg ()
  '(or string symbol))

(deftype structure ()
  '(satisfies structurep))

(deftype unbound-variable-marker ()
  '(satisfies unbound-variable-marker-p))

(deftype byte-specifier ()
  '(satisfies byte-p))

(deftype short-float () 'float)

(deftype single-float () 'float)

(deftype double-float () 'float)

(deftype long-float () 'float)

(deftype string-char ()
  'character)

(deftype bit ()
  `(integer 0 1))

(deftype mod (n) `(integer 0 (,n)))

(deftype signed-byte (&optional width)
  (if (eq width '*)
      'integer
      (let ((x (expt 2 (1- width))))
	`(integer ,(- x) ,(- x 1)))))

(deftype unsigned-byte (&optional width)
  `(integer 0 ,(if (eq width '*)
		   width
		   (- (expt 2 width) 1))))

(deftype pathname ()
  'physical-pathname)

