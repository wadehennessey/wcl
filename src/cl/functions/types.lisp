;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun-inline 32bit-vector-p (x)
  (%tag_mask= x vector-mask #x38))

(defun-inline adjustable-array-p (x)
  (complex-array-p x))

(defun-inline array-has-fill-pointer-p (a)
  (vector-with-fill-pointer-p a))

;;; Return element-type-tag,  element-size, and default initialize-elements
(defun type->element-type-tag (element-type)
  (let* ((exp-type (type-macroexpand element-type))
	 (type (if (atom exp-type) (list exp-type) exp-type)))
    (case (first type)
      (integer (let ((low (second type))
		     (high (third type)))
		 (if (and (numberp low) (numberp high))
		     (if (>= low 0)
			 (cond ((= high 1)
				(values element-type-bit 1 0))
			       ((<= high (1- (expt 2 8)))
				(values element-type-unsigned-8bit 8 0))
			       ((<= high (1- (expt 2 16)))
				(values element-type-unsigned-16bit 16 0))
			       ((<= high (1- (expt 2 32)))
				(values element-type-unsigned-32bit 32 0))
			       (t (values element-type-ptr 32)))
			 (cond ((and (>= low (- (expt 2 7)))
				     (<= high (1- (expt 2 7))))
				(values element-type-signed-8bit 8 0))
			       ((and (>= low (- (expt 2 15)))
				     (<= high (1- (expt 2 15))))
				(values element-type-signed-16bit 16 0))
			       ((and (>= low (- (expt 2 31)))
				     (<= high (1- (expt 2 31))))
				(values element-type-signed-32bit 32 0))
			       (t (values element-type-ptr 32 nil))))
		     (values element-type-ptr *target-bits-per-word* 0))))
      (float (values element-type-float 64 0.0))
      (character (values element-type-char 8 #\Null))
      ((t array simple-array symbol or and not member satisfies)
       ;; latest:        (values element-type-ptr *target-bits-per-word* nil)
       (values element-type-ptr 32 nil))
      (t (error "~A is not a legal element-type" element-type)))))


(defun rewrite-typep (form any-type)
  (complete-delayed-defstructs)
  (let ((mexp-type (type-macroexpand any-type)))
    (cond ((eq mexp-type t) t)
	  ((null mexp-type) nil)
	  (t (let* ((type (if (atom mexp-type) (list mexp-type) mexp-type))
		    (second (if (null (second type)) '* (second type)))
		    (third (if (null (third type)) '* (third type))))
	       (case (first type)
		 (satisfies `(,(second type) ,form))
		 ((simple-array array)
		  (rewrite-array-typep form type second third))
		 (integer
		  (if (null (cdr type))
		      `(integerp ,form)
		      (if (and (fixnump second) (fixnump third))
			  (cond ((and (= second most-negative-fixnum)
				      (= third most-positive-fixnum))
				 `(fixnump ,form))
				((and (= second 0) (= third 1)) `(bitp ,form))
				(t `(integer-range-p ,form ',second ',third)))
			  `(integer-range-p ,form ',second ',third))))
		 (float `(floatp ,form))
		 (character `(characterp ,form))
		 (symbol `(symbolp ,form))
		 (or (cond ((equal type '(or integer float ratoi complex))
			    `(numberp ,form))
			   ((equal type '(or null cons)) `(listp ,form))
			   (t (rewrite-combined-type form type))))
		 ((and not) (rewrite-combined-type form type))
		 (member `(member ,form ',(cdr mexp-type)))
		 (t (let ((info (gethash (first type) *structure-info*)))
		      (if (or (null info)
			      (not (struct-info-named? info)))
			  (multiple-value-bind (new-type rewrite?)
			      (type-macroexpand type)
			    (if rewrite?
				(rewrite-typep form new-type)
				(values nil t))) ; indicate failure
			  `(,(predicate-name info) ,form))))))))))

(defun rewrite-combined-type (form type)
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,form))
       ,(cons (first type) (mapcar #'(lambda (x)
				       (rewrite-typep tmp x))
				   (rest type))))))

(defun rewrite-array-typep (form type second third)
  (flet ((pred (name)
	   (if (eq (first type) 'simple-array)
	       (intern (concatenate 'string "SIMPLE-" (string name))
		       *lisp-package*)
	       name)))
    (let ((element-type (if (eq second '*) t second)))
      (if (null (cdr type))
	  `(,(pred 'array-p) ,form)
	  (if (and (listp third) (= (length third) 1))
	      ;; Special case vectors
	      (let ((type-code (type->element-type-tag element-type)))
		(select type-code
			(element-type-char
			 `(,(pred 'string-p) ,form))
			(element-type-float
			 `(,(pred 'float-vector-p) ,form))
			(element-type-bit
			 `(,(pred 'bit-vector-p) ,form))
			(element-type-signed-8bit
			 `(,(pred 'signed-8bit-vector-p) ,form))
			(element-type-unsigned-8bit
			 `(,(pred 'unsigned-8bit-vector-p) ,form))
			(element-type-signed-16bit
			 `(,(pred 'signed-16bit-vector-p) ,form))
			(element-type-unsigned-16bit
			 `(,(pred 'unsigned-16bit-vector-p) ,form))
			(element-type-signed-32bit
			 `(,(pred 'signed-32bit-vector-p) ,form))
			(element-type-unsigned-32bit
			 `(,(pred 'unsigned-32bit-vector-p) ,form))
			(t `(,(pred 'vector-p) ,form))))
	      `(,(pred 'array-type-p) ,form ',element-type ',third))))))

(defun type-macroexpand (form &optional local-macro-env already-expanded?)
  (let ((list-form (if (listp form) form (list form))))
    (multiple-value-bind (new-form expanded?)
	(expand-macro list-form
		      *type-macro-expanders*
		      local-macro-env
		      t
		      nil)
      (if expanded?
	  (type-macroexpand new-form local-macro-env t)
	  (values (expand-inner-types form)
		  (or already-expanded? expanded?))))))
	  
(defun expand-inner-types (type)
  (if (and (listp type)
	   (or (eq (first type) 'array)
	       (eq (first type) 'simple-array))
	   (not (null (second type))))
      `(,(first type)
	,(type-macroexpand (second type))
	,@(or (cddr type) '(*)))
      type))

(defun array-type-p (a type dims)
  (and (case (car a)
	 (simple-array (simple-array-p a))
	 (simple-vector (simple-vector-p a))
	 (array (%tag_mask= a 1 0))
	 (vector (%tag_mask= a 3 0)))
       (or (eq type '*)
	   (%tag_mask= a
		       array-element-type-mask
		       (type->element-type-tag type)))
       (or (eq dims '*)
	   (let ((len (length dims)))
	     (and (= (length dims) (array-rank a))
		  (loop for i from 0 below len
			for dim in dims
			unless (cond ((eq dim '*) t)
				     ((numberp dim)
				      (= (array-dimension a i) dim))
				     (t (error "Dimension ~A is not a number"
					       dim)))
			do (return nil)
			finally (return t)))))))

(defun arrayp (x)
  (%tag_mask= x #b1 array-tag))

(defun-inline array-p (x)
  (arrayp x))

(defun assert-error (test-form)
  (error "ASSERT failed on test ~A" test-form))

(defun-inline atom (x)
  (not (consp x)))

(defun-inline bignump (x)
  (%tag= x type-bignum))

(defun-inline bit-array-p (x)
  (%tag_mask= x array-mask #x0))

(defun-inline bit-vector-p (x)
  (%tag_mask= x vector-mask #x0))

(defun-inline bitp (x)
  (or (%= x 0) (%= x 1)))

(defun-inline characterp (x)
  (%tag= x type-character))

(defun check-type-error (place value type &optional string)
  (error "The value ~A is not of type ~A" value type))

(defun coerce-to-list (s)
  (typecase s
    (vector (loop for i from 0 below (length s)
		  collect (aref s i)))
    (list s)))

(defun coerce-to-string (s)
   (etypecase s
     (list (loop with len = (length s)
		 with string = (make-string len)
		 for c in s
		 for i from 0 below len 
		 do (setf (schar string i) c)
		 finally (return string)))
     (t (string s))))

(defun coerce-to-vector (s element-type)
  (loop with len = (length s)
	with vector = (make-array len :element-type element-type)
	for i from 0 below len 
	do (setf (elt vector i) (elt s i))
	finally (return vector)))

(defun character (x)
  (if (and (fixnump x) (< x char-code-limit))
      (code-char x)
      (if (and (or (symbolp x) (stringp x))
	       (= 1 (length (string x))))
	  (schar (string x) 0)
	  (error "Cannot coerce ~S into a character object" x))))

;;; HEY! this needs more work to handle all required coercions correctly.
(defun coerce (x type)
  (flet ((fail ()
	   (error "Unable to coerce ~S to type ~S" x type)))
    (let ((basic-type (type-macroexpand type)))
      (cond ((atom basic-type) (case basic-type
				 (float (float x))
				 (character (character x))
				 ((t) x)
				 (t (fail))))
	    ((equal basic-type '(or null cons)) (coerce-to-list x))
	    ((equal basic-type '(satisfies complexp))
	     (if (numberp x)
		 (complex x)
		 (fail)))
	    ((and (member (car basic-type) '(array simple-array))
		  (= (length (third basic-type)) 1)) ; vector?
	     (let ((element-type (second basic-type)))
	       (if (or (listp x) (vectorp x))
		   (case element-type
		     (character (coerce-to-string x))
		     (* (coerce-to-vector x t))
		     (t (coerce-to-vector x element-type)))
		   (fail))))
	    (t (fail))))))


(defun compiled-function-p (x)
  (procedurep x))

(defun-inline complex-array-p (x)
  (%tag_mask= x #b101 #b100))

(defun complex-vector-p (x)
  (%tag_mask= x #b111 #b100))

(defun-inline complexp (x)
  (%tag= x type-complex))

(defun-inline consp (x)
  (%tag= x type-cons))

(defun-inline eq (x y)
  (%eq x y))

(defun-inline eql (x y)
  (c_eql x y))

(defun equal (x y)
  (or (eql x y)
      (typecase x
	(simple-string (and (simple-string-p y) (string= x y)))
	(cons (and (consp y)
		   (equal (car x) (car y))
		   (equal (cdr x) (cdr y))))
	(pathname (and (pathnamep y)
		       (equal (pathname-host x) (pathname-host y))
		       (equal (pathname-device x) (pathname-device y))
		       (equal (pathname-directory x) (pathname-directory y))
		       (equal (pathname-name x) (pathname-name y))
		       (equal (pathname-type x) (pathname-type y))))
	(t nil))))

;;; HEY! fix this. Needs struct equality and string equality tests.
(defun equalp (x y)
  (equal x y))

(defun-inline fixnump (x)
  (%fixnump x))

(defun-inline floatp (x)
  (%tag= x type-float))

(defun-inline foreign-pointer-p (x)
  (%tag= x type-foreign-ptr))

(defun-inline functionp (x)
  (or (symbolp x) (procedurep x)))

(defun integer-range-p (x low high)
  (and (integerp x)
       (or (eq low '*) (>= x low))
       (or (eq high '*) (<= x high))))

(defun-inline integerp  (x)
  (or (fixnump x) (bignump x)))

(defun-inline listp (x)
  (or (null x) (consp x)))

(defun-inline not (x)
  (eq x nil))

(defun-inline null (x)
  (eq x nil))

(defun-inline numberp (x)
  (%numberp x))

(defun-inline procedurep (x)
  (%tag= x type-procedure))

(defun-inline rationalp (x)
  (integerp x))

(defun-inline realp (x)
  (or (rationalp x) (floatp x)))

(defun-inline ratiop (x)
  (%tag= x type-ratio))

(defun-inline simple-1d-array-p (x)
  (%tag_mask= x #b111 #b000))

(defun-inline simple-array-p (x)
  (%tag_mask= x #b101 #b000))

(defun-inline simple-bit-array-p (x)
  (%tag_mask= x array-mask #x0))

(defun-inline simple-bit-vector-p (x)
  (%tag= x type-simple-bit-vector))

(defun-inline simple-signed-8bit-vector-p (x)
  (%tag= x type-simple-signed-8bit-vector))

(defun-inline simple-unsigned-8bit-vector-p (x)
  (%tag= x type-simple-unsigned-8bit-vector))

(defun-inline simple-signed-16bit-vector-p (x)
  (%tag= x type-simple-signed-16bit-vector))

(defun-inline simple-unsigned-16bit-vector-p (x)
  (%tag= x type-simple-unsigned-16bit-vector))

(defun-inline simple-signed-32bit-vector-p (x)
  (%tag= x type-simple-signed-32bit-vector))

(defun-inline simple-unsigned-32bit-vector-p (x)
  (%tag= x type-simple-unsigned-32bit-vector))

(defun-inline simple-float-vector-p (x)
  (%tag= x type-simple-float-vector))

(defun-inline simple-string-p (x)
  (%tag= x type-simple-string))

(defun-inline simple-vector-p (x)
  (%tag= x type-simple-vector))

(defun-inline string-char-p (x)
  (characterp x))

(defun stringp (x)
  (or (simple-string-p x)
      (and (vectorp x)
	   (= (element-type-tag x) element-type-char))))

(defun-inline string-p (x)
  (stringp x))

(defun-inline structurep (x)
  (%tag= x type-structure))

(defun unbound-variable-marker-p (x)
  (%tag= x type-ubv))

;;; HEY! This SUBTYPE implementation is a pretty rough, needs some work,
;;; especially now that the rest of the type system is in better shape.
(defparameter *simple-subtypes*
  '((complex number)
    (ratio rational number)
    (float number)
    (number)
    (symbol)
    (null list)
    (cons list)
    (atom)
    (function) 
    (compiled-function function)
    (procedure function)
    (structure)
    (character)))

(defconstant combined-type-ops '(and or not member satisfies))

(defun subtypep (type1 type2)
  (let ((t1 (type-macroexpand type1))
	(t2 (type-macroexpand type2)))
    (cond ((eq t2 t) (values t t))	
	  ((eq t2 nil) (values nil t))
	  ((eq t1 t2) (values t t))
	  (t (let ((normal-t1 (normalize-type t1))
		   (normal-t2 (normalize-type t2)))
	       (cond ((combined-type t1)
		      (combined-t1-subtypep normal-t1 normal-t2))
		     ((combined-type t2)
		      (combined-t2-subtypep normal-t1 normal-t2))
		     (t (simple-subtypep normal-t1 normal-t2))))))))

;;; HEY! This should be obsolete now that types expande correctly.
(defun normalize-type (type)
  (let ((key (if (atom type) type (car type))))
    (case key
      (integer (cond ((or (atom type) (null (cdr type))) '(integer * *))
		     ((null (cddr type)) `(integer ,(second type) *))
		     (t type)))
      (string-char 'character)		; CLTL1 compat
      ((array simple-array)
       (cond ((or (atom type) (null (cdr type))) `(,key * *))
	     ((null (cddr type) `(,key ,(second type) *)))
	     (t type)))
      (complex `(complex ,@(if (and (consp type) (not (null (cdr type))))
			      (cdr type)
			      '(number))))
      (t type))))

(defun memq-t (x l)
  (if (member x l) t nil))

(defun atomic-t1-subtypep (t1 t2)
  (values (let ((l (assoc t1 *simple-subtypes*)))
	    (if (null l)
		(let ((t1-info (lookup-structure-info t1))
		      (t2-info (and (atom t2) (lookup-structure-info t2))))
		  (if (or (null t1-info) (null t2-info))
		      nil
		      (structure-predicate-1 t2-info t1)))
		(memq-t t2 l)))
	  t))

(defun simple-subtypep (t1 t2)
  (if (atom t1)
      (atomic-t1-subtypep t1 t2)
      (case (car t1)
	(integer (integer-subtypep t1 t2))
	(float (float-subtypep t1 t2))
	((array simple-array)
	 (array-subtypep t1 t2))
	;; HEY! use STRUCTURE-SUBTYPEP here
	(complex (complex-subtypep t1 t2))
	(t (error "unknown type ~A" t1)))))

(defun float-subtypep (t1 t2)
  ;; HEY! PUNT for now.
  (values nil
	  t))

(defun combined-type (type)
  (and (consp type) (member (car type) combined-type-ops)))

(defun loose->= (x y)
  (or (eq x '*) (eq y '*) (>= x y)))

(defun loose-<= (x y)
  (or (eq x '*) (eq y '*) (<= x y)))

(defun integer-subtypep (range-1 t2)
  (values (or (and (listp t2)
		   (eq (car t2) 'integer)
		   (loose->= (second range-1) (second t2))
		   (loose-<= (third range-1) (third t2)))
	      (memq-t t2 '(rational number)))
	  t))

;;; HEY! I'm sure this needs more work...it's too simple right now.
(defun array-subtypep (t1 t2)
  (destructuring-bind (a1 a1-type a1-dims) t1
    (destructuring-bind (a2 a2-type a2-dims) t2
      (values (and (or (eq a1 a2)
		       (eq a1 'simple-array)
		       (eq a2 'array))
		   (array-element-subtypep a1-type a2-type)
		   (array-dims-subtypep a1-dims a2-dims))
	      t))))

(defun array-element-subtypep (t1 t2)
  (or (eq t2 '*)
      (eq t1 t2)
      (and (not (eq t1 't))
	   (not (eq t2 't))
	   ;; HEY! FIX this - at least for bit arrays and strings
	   (subtypep t1 t2))))

(defun array-dims-subtypep (dims-1 dims-2)
  (or (eq dims-2 '*)
      (and (= (length dims-1) (length dims-2))
	   (every #'(lambda (d1 d2)
		      (or (eq d1 '*) (eq d2 '*) (= d1 d2)))
		  dims-1
		  dims-2))))

(defun complex-subtypep (t1 t2)
  (if (atom t2)
      (values (eq t2 'number) t)
      (let ((x1 (second t1))
	    (x2 (second t2)))
	(or (eq x2 '*)
	    (subtypep x1 x2)))))

(defun combined-t1-subtypep (t1 t2)
  (flet ((inner-subtypep (st1)
	   (multiple-value-bind (flag ok?) (subtypep st1 t2)
	     (if ok?
		 flag
		 (return-from combined-t1-subtypep (values nil nil))))))
    (case (car t1)
      (and (values (some #'inner-subtypep (cdr t1)) t))
      (or (values (every #'inner-subtypep (cdr t1)) t))
      ((not member satisfies) (values nil nil)))))

(defun combined-t2-subtypep (t1 t2)
  (flet ((inner-subtypep (st2)
	   (multiple-value-bind (flag ok?) (subtypep t1 st2)
	     (if ok?
		 flag
		 (return-from combined-t2-subtypep (values nil nil))))))
    (case (car t2)
      (and (values (every #'inner-subtypep (cdr t2)) t))
      (or (values (some #'inner-subtypep (cdr t2)) t))
      ((not member satisfies) (values nil nil)))))

(defun-inline symbolp (s)
  (%tag= s type-symbol))
;;;  (%tag_mask= x symbol-tag-mask either-symbol-tag)

(defun-inline line-symbol-p (s)
  (%tag= s type-line-symbol))

(defun-inline real-symbol-p (s)
  (%tag= s type-symbol))

(defun make-line-symbol (real-symbol line)
  (let ((line-symbol (alloc_words 3 type-line-symbol)))
    (%32bit-def line-symbol line-symbol-line-offset line)
    (%32bit-def line-symbol symbol-self-link-offset real-symbol)
    line-symbol))

(defun line-symbol-line (s)
  (%32bit-ref s line-symbol-line-offset))

(defun line-symbol-symbol (s)
  (%32bit-ref s symbol-self-link-offset))

(defun type-of (x)
  (typecase x
    (fixnum 'fixnum)
    (bignum 'bignum)
    (float 'float)
    (symbol 'symbol)
    (string-char 'string-char)
    (simple-string 'simple-string)
    (function 'function)
    (cons 'cons)
    (structure (structure-type x))
    (simple-vector 'simple-vector)
    (simple-array 'simple-array)
    (vector 'vector)
    (array 'array)
    (ratio 'ratio)
    (complex 'complex)
    (byte-specifier 'byte-specifier)
    (foreign-pointer 'foreign-pointer)
    (t (error "HEY! Finish type-of"))))

(defun typep (x type)
  (multiple-value-bind (rewrite unknown-type?)
      (rewrite-typep (list 'quote x) type)
    (if unknown-type?
	(error "The type ~A is not defined" type)
	(eval rewrite))))

(defun-inline vector-with-fill-pointer-p (x)
  (%tag_mask= x #b10000111 #b10000100))

(defun-inline vectorp (x)
  (%tag_mask= x vector-tag-mask vector-tag))

(defun-inline vector-p (x)
  (vectorp x))

(defun wta (arg type position)
  (declare (ignore position))
  (error "~S is not a ~A" arg type))

