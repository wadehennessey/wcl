;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; Emit object x and all of it's sub-objects.
;;; Returns a string of C code which references the object.
(defun emit-data (x)
  (cond ((fixnump x) nil)
	((characterp x) (format nil "char_tab[~D]" (char-code x)))
	(t (let ((label (gethash x *const-labels*)))
	     (if (null label)
		 (typecase x
		   (symbol
		    (let ((label (lisp->c-symbol-name x)))
		      (if (null *emit-symbol-data-function*)
			  (emit-win ":sym ~S~%" x)
			  (funcall *emit-symbol-data-function* x))
		      (emit-k "extern SYMBOL ~A; ~%" label)
		      (setf (gethash x *const-labels*) label)))
		   (t (let ((label (genstring "k")))
			;; for circ consts
			(setf (gethash x *const-labels*) label) 
			(etypecase x
			  (vector (emit-vector label x))
			  (array (emit-multi-array label x))
			  (inner-proc
			   (emit-null-oe-proc label x))
			  (foreign-symbol (emit-foreign-symbol x))
			  (cons (emit-cons label x))
			  (float (emit-float label x))	
			  (ratio (emit-ratio label x))
			  (complex (emit-complex label x))
			  (compiled-function
			   (emit-compiled-function label x))
			  (structure (emit-structure label x))
			  (integer (emit-bignum label x)))
			label)))
		 label)))))

(defun emit-lref (x)
  (if (fixnump x)
      (format nil "(LP) ~D" (ash x 1))
      (format nil "LREF(~A)" (emit-data x))))

(defun emit-foreign-symbol (x)
  x (error "fix"))

(defun emit-float (label x)
  (emit-k "MAKE_FLOAT(~A,~F);~%" label x))

(defun emit-ratio (label x)
  (let ((numerator (emit-lref (numerator x)))
	(denominator (emit-lref (denominator x))))
  (emit-k "MAKE_RATIO(~A,~A,~A);~%" label numerator denominator)))

(defun emit-complex (label x)
  (let ((real (emit-lref (realpart x)))
	(imag (emit-lref (imagpart x))))
  (emit-k "MAKE_COMPLEX(~A,~A,~A);~%" label real imag)))

(defun emit-cons (label x)
  (let ((car (emit-lref (car x)))
	(cdr (emit-lref (cdr x))))
    (emit-k "MAKE_CONS(~A,~A,~A);~%" label car cdr)))

(defun emit-null-oe-proc (label x)
  (emit-k "MAKE_PROCEDURE(~A,~A);~%" label (proc-c-name x)))

(defun emit-vector (label x)
  (cond ((simple-string-p x) (emit-simple-string label x))
	((simple-array-p x) (emit-simple-1d-array label x))
	(t (emit-complex-1d-array label x))))

(defun emit-multi-array (label x)
  (if (simple-array-p x)
      (emit-simple-multi-array label x)
      (emit-complex-multi-array label x)))

(defun emit-string-using-c-syntax (string)
  (write-char #\" *k-stream*)
  (loop for c being the array-elements of string do
	(case c
	  (#\newline (write-string "\\n" *k-stream*))
	  (#\\ (write-string "\\\\" *k-stream*))
	  (#\" (write-string "\\\"" *k-stream*))
	  (t (write-char c *k-stream*))))
  (write-char #\" *k-stream*))

(defun emit-simple-string (label x)
  (let ((len (length x)))
    (if (< len 80)
	(progn (emit-k "MAKE_SIMPLE_STRING(~A,~D," label len)
	       (emit-string-using-c-syntax x)
	       (emit-k ");~%"))
	;; ARGHH!!! We cannot use MAKE_SIMPLE_STRING because the losing
	;; MIPS cpp(1.31) will only pass strings of <= 80 chars as args!!!
	(progn
	  (emit-k "static struct {unsigned long header; char string[~D+1];}~%"
		  len)
	  (emit-k "~A  = {((~D << 8) + TYPE_SIMPLE_STRING), ~%"label len)
	  (emit-string-using-c-syntax x)
	  (emit-k "};~%")))))

(defun emit-simple-1d-array (label x)
  (multiple-value-bind (element-type-tag element-size default-initial-value)
      (type->element-type-tag (array-element-type x))
    (declare (ignore default-initial-value))
    (let* ((objects (if (= element-type-tag element-type-bit)
			(bit-vector->word-list x)
			(loop for e being the elements of x
			      collect (if (= element-type-tag element-type-ptr)
					  (emit-lref e)
					  e))))
	   (object-len (length objects)))
      (declare (ignore default-initial-value))
      ;; Sun CC doesn't like 0 element arrays, so we include an unused
      ;; element (gcc doesn't seem to care). This is consistent with
      ;; zero length heap allocated vectors, although static vectors
      ;; don't really need the extra space since they will never be
      ;; converted to a forwarding pointer.
      (let ((c-type  (select element-size
		       (1 "unsigned long")
		       (8 "unsigned char")
		       (16 "unsigned short")
		       (32 "LP")
		       (64 "unsigned double"))))
	(emit-k "static struct {unsigned long header; ~A cells[~D];} ~%"
		c-type
		(if (= object-len 0) 1 object-len))
	(emit-k "~A = {0x~X, ~%{"
		label
		(+ (ash (length x) 8) element-type-tag))
	(if (= object-len 0)
	    (emit-k " 0 ")
	    (loop for rest on objects
		  unless (eq rest objects) do (emit-k ",")
		  do (emit-k "((~A)~A)" c-type (car rest))))
	(emit-k "}};~%")))))

;;; This should be byte-order independent, since words are words....
(defun bit-vector->word-list (bit-vector)
  (loop with word-len = (/ (object-size bit-vector) 4)
	with word-vector = (make-array word-len
				       :displaced-to bit-vector
				       :element-type '(unsigned-byte 32))
	for i from 0 below word-len
	collect (aref word-vector 0)))

(defun emit-complex-1d-array (label x)
  label x
  (error "write complex-1d array emitter"))

(defun emit-complex-multi-array (label x)
  label x
  (error "write complex multi-array emitter"))

(defun emit-simple-multi-array (label x)
  (let ((header (object-header x))
	(underlying-vector (emit-lref (array-underlying-vector x)))
	(dims-vector (emit-lref (array-dims-vector x)))
	(multiplier-vector (emit-lref (array-multiplier-vector x))))
    (emit-k "static SIMPLE_MULTI_ARRAY ~A = " label)
    (emit-k "{0x~X, ~A, ~A, ~A};~%"
	    header
	    underlying-vector
	    dims-vector
	    multiplier-vector)))

(defun emit-compiled-function (label x)
  (let ((c-name (lisp->c-proc-name (compiled-function-name x))))
    (emit-k "~%extern LP ~A();~%" c-name)
    (emit-k "MAKE_PROCEDURE(~A,~A);~%" label c-name)))

(defun emit-structure (label s)
  (let* ((name (type-of s))
	 (info (lookup-structure-info name)))
    (if (null info)
	(error "No structure info found for ~A" name)
	(let* ((len (struct-info-length info))
	       (type-label (emit-lref (struct-info-name info)))
	       (objects (loop for i from 0 below (1- len)
			      collect (emit-lref
				       (ref-structure-as-vector s i)))))
	  (emit-k "static struct {unsigned long header; LP type;")
	  (emit-k "LP cells[~D];} ~%" len)
	  (emit-k "~A = {((~D << 8) + TYPE_STRUCTURE), ~A,~%{"
		  label len type-label)
	  (loop for rest on objects
		unless (eq rest objects) do (emit-k ",")
		do (emit-k "~A" (car rest)))
	  (emit-k "}};~%")))))

(defun emit-bignum (label n)
  (let* ((abs (abs n))
	 (hex-digits (write-to-string abs :base 16))
	 (len (length hex-digits))
	 (bits-per-hex-digit 4)
	 (initializer-width (/ *target-bits-per-word* bits-per-hex-digit))
	 (bits-len (ceiling len initializer-width)) ; in words
	 (bytes-per-word (/ *target-bits-per-word* 8))
	 (bytes-per-int 4)		; same on 32 or 64 bit x86
	 (header-len (+ (* bits-len bytes-per-word) ; Header length is bytes
			(* 2 bytes-per-int)
			bytes-per-word)) ; data ptr
	 (header (+ (ash header-len 8) type-bignum)))
    (emit-k
     "static struct {BIGNUM_HEADER header; unsigned long bits[~D];} "
     bits-len)
    (emit-k"~A = ~%{{0x~X, ~D, ~D, &(~A.bits[0])}, {"
	   label
	   header
	   bits-len
	   (* bits-len (if (> n 0) 1 -1))
	   label)
    (loop for high from len downto 0 by initializer-width
	  for i from (1- bits-len) downto 0
	  do (progn (emit-k "0x~X~A"
			    (subseq hex-digits
				    (max (- high initializer-width) 0)
				    high)
			    (if (= i 0) "" ","))))
    (emit-k "}};~%")))



