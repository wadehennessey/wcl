;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun installation-parameter (name)
  (with-open-file (input (configuration-file-name))
    (loop for line = (read-line input nil input)
	  until (eq line input)
	  when (string= name (string-right-trim
			      '(#\Space)
			      (subseq line 0 (position #\= line))))
	  do (return (read-from-string (subseq line (1+ (position #\= line)))))
          finally (warn "Installation Parameter ~A not found." name))))

(defun configuration-file-name ()
  (or (getenv "WCL_CONFIGURATION") 
      (format nil "~A/CONFIGURATION" *root-directory*)))

(defmacro def-nary-optimizer (name 2-case)
  `(define-compiler-macro ,name (&whole form x &rest others)
    (let ((n (length others)))
      (case n
	(0 x)
	(1 `(,',2-case ,x ,(first others)))
	(t form)))))

(defmacro def-integer-arith-op/2 (name op)
  `(defprimitive ,name ((x t) (y t) => (result t))
    (emit-c "((LP) ((long) ~A ~A (long) ~A))" x ',op y)))

(defmacro def-float-arith-op/2 (name op)
  `(defprimitive ,name ((x double) (y double) => (result double))
    (emit-c "((double) ~A ~A (double) ~A)" x ',op y)))

(defmacro def-integer-arith-predicate/2 (name op)
  `(defprimitive ,name ((x t) (y t) => (flag if-test))
    (emit-c "((long) ~A ~A (long) ~A)" x ',op y)))

(defmacro def-float-arith-predicate/2 (name op)
  `(defprimitive ,name ((x double) (y double) => (flag if-test))
    (emit-c "((double) ~A ~A (double) ~A)" x ',op y)))

(defun dyadicize-rl (bin-func args)
  (if (null (cddr args))
      (cons bin-func args)
      `(,bin-func ,(first args) ,(dyadicize-rl bin-func (rest args)))))

(defun dyadicize-lr (bin-func args)
  (if (null (cddr args))
      (cons bin-func args)
      `(,bin-func ,(dyadicize-lr bin-func (butlast args 1)) ,@(last args))))

(defun test-function (test test-not)
  (if (null test-not)
      test
      `(complement/2 ,test-not)))

(defun simple-eq-version? (test test-not key)
  (and (or (equal test '(function eq)) (eq test 'eq))
       (or (equal key '(function identity)) (eq key 'identity))
       (null test-not)))

(defun simple-eql-version? (test test-not key)
  (and (or (equal test '(function eql)) (eq test 'eql))
       (or (equal key '(function identity)) (eq key 'identity))
       (null test-not)))

(defun rewrite-mapping-form (form stepper collector)
  (destructure ((function . arg-lists) (cdr form))
    (let ((vars (n-list (length arg-lists) #'(lambda () (gensym "L")))))
      `(loop ,@(loop for var in vars
		for arg-list in arg-lists
		appending (case stepper
			    (car `(for ,var in ,arg-list))
			    (cdr `(for ,var on ,arg-list by #'car))))
	,(case collector
	   (cons 'collect)
	   (nconc 'nconcing)
	   (nothing 'do))
	(funcall ,function ,@vars)))))

(defun improve-writer (function args options)
  `(let ,(loop for (option . var) in '((:escape . *print-escape*)
				       (:radix . *print-radix*)
				       (:base . *print-base*)
				       (:circle . *print-circle*)
				       (:pretty . *print-pretty*)
				       (:level . *print-level*)
				       (:length . *print-length*)
				       (:case . *print-case*)
				       (:gensym . *print-gensym*)
				       (:array . *print-array*)
				       (:readably  . *print-readably*)
				       (:right-margin . *print-right-margin*)
				       (:miser-width *print-miser-width*)
				       (:lines . *print-lines*)
				       (:pprint-dispatch .
					*print-pprint-dispatch*))
	  when (member option options :test #'eq)
	  collect (list var (second (member option options
					    :test #'eq))))
    (,function ,@args)))

(defun optimize-simple-member (form test x list)
  (declare (ignore test x list))
  form)


(defun initialize-compiler-macros ()
  (define-compiler-macro typep (&whole orig form type)
    (if (quoted-constant-p type)
	(multiple-value-bind (rewrite unknown-type?)
	    (rewrite-typep form (second type))
	  (if unknown-type?
	      (progn (warn "Unknown type: ~A" (second type))
		     orig)
	      rewrite))
	orig))

  (define-compiler-macro + (&whole form &rest args)
    (case (length args)
      (0 0)
      (1 (car args))
      (2 `(add ,@args))
      (t (dyadicize-lr 'add args))))

  (define-compiler-macro - (&whole form &rest args)
    (case (length args)
      (0 0)
      (1 `(subtract 0 ,(first args)))
      (2 `(subtract ,@args))
      (t (dyadicize-lr 'subtract args))))

  (define-compiler-macro * (&whole form x &rest others)
    (case (length others)
      (0 x)
      (1 `(multiply ,x ,(first others)))
      (t (dyadicize-lr 'multiply (cdr form)))))

  (define-compiler-macro / (&whole form x &rest others)
    (case (length others)
      (0 x)
      (1 `(divide ,x ,(first others)))
      (t (dyadicize-lr 'divide (cdr form)))))

  (def-nary-optimizer >= geq_p)
  (def-nary-optimizer > greaterp)
  (def-nary-optimizer <= leq_p)
  (def-nary-optimizer < lessp)
  (def-nary-optimizer = num_equal_p)

  (def-nary-optimizer char= char=/2)
  (def-nary-optimizer char>= char>=/2)
  (def-nary-optimizer char> char>/2)
  (def-nary-optimizer char<= char<=/2)
  (def-nary-optimizer char< char</2)

  (def-nary-optimizer logand logand/2)
  (def-nary-optimizer logior logior/2)

  ;;(def-nary logeqv logeqv/2)
  ;;(def-nary logxor logxor/2)
  ;;(def-nary max max/2)
  ;;(def-nary min min/2)

  ;; Some other math macros are in the rtl files - really?????

  (define-compiler-macro append (&whole form &rest args)
    (case (length args)
      (0 nil)
      (1 (first args))
      (2 `(append/2 ,@args))
      (t (dyadicize-rl 'append/2 args))))

  (define-compiler-macro nconc (&whole form &rest args)
    (case (length args)
      (0 nil)
      (1 (first args))
      (2 `(nconc/2 ,@args))
      (t (dyadicize-rl 'nconc/2 args))))

  (define-compiler-macro write (form &rest options
				       &key (stream '*standard-output*)
				       &allow-other-keys)
    (improve-writer 'write-object (list form stream) options))

  (define-compiler-macro write-to-string (form &rest options)
    (improve-writer 'write-to-string-1 (list form) options))


  (define-compiler-macro complement/2 (function)
    `#'(lambda (x y) (not (funcall ,function x y))))

  (define-compiler-macro member (&whole form x list &key
					  (test '#'eql)
					  test-not
					  (key '#'identity))
    (cond ((simple-eq-version? test test-not key) `(memq ,x ,list))
	  ((simple-eql-version? test test-not key) `(memql ,x ,list))
	  (t `(member/4 ,x ,list ,(test-function test test-not) ,key))))

  (define-compiler-macro assoc (x a-list &key
				    (test '#'eql) test-not (key '#'identity))
    (cond ((simple-eq-version? test test-not key) `(assq ,x ,a-list))
	  ((simple-eql-version? test test-not key) `(assql ,x ,a-list))
	  (t `(assoc/4 ,x ,a-list ,(test-function test test-not) ,key))))

  (define-compiler-macro memql (&whole form x list)
    (optimize-simple-member form 'eql x list))

  (define-compiler-macro memq (&whole form x list)
    (optimize-simple-member form 'eql x list))

  (define-compiler-macro find (&whole form x sequence &rest stuff)
    (if (and (null stuff)
	     (quoted-constant-p sequence))
	(cond ((< (length (second sequence)) 4)
	       (let ((key (gensym "KEY")))
		 `(let ((,key ,x))
		   (or ,@(loop for e in (second sequence)
			  collect `(eql ,key ',e))))))
	      ((every #'symbolp (second sequence))
	       (find->hashed-symbol-lookup x (second sequence)))
	      (t form))
	form))

  (define-compiler-macro case (&whole form key &rest cases)
    (optimize-case form key cases))

  (define-compiler-macro typecase (&whole form key &rest cases)
    (optimize-typecase form key cases))

  (define-compiler-macro mapcar (&whole form)
    (rewrite-mapping-form form 'car 'cons))

  (define-compiler-macro maplist (&whole form)
    (rewrite-mapping-form form 'cdr 'cons))

  (define-compiler-macro mapcan (&whole form)
    (rewrite-mapping-form form 'car 'nconc))

  (define-compiler-macro mapcon (&whole form)
    (rewrite-mapping-form form 'cdr 'nconc))

  (define-compiler-macro mapc (&whole form)
    (rewrite-mapping-form form 'car 'nothing))

  (define-compiler-macro mapl (&whole form)
    (rewrite-mapping-form form 'cdr 'nothing))

  (define-compiler-macro symbol-value (&whole orig name)
    (if (config-inline-calls? *config*)
	`(unsafe-symbol-value ,name)
	orig))

  (define-compiler-macro symbol-function (&whole orig name)
    (if (config-inline-calls? *config*)
	`(unsafe-symbol-function ,name)
	orig))

  (define-compiler-macro make-array (&whole orig
					      dims &key	
					      (element-type 't)
					      (initial-element nil ie?)
					      initial-contents
					      adjustable
					      fill-pointer
					      displaced-to
					      displaced-index-offset)
    (if (and (constantp element-type)
	     (null adjustable)
	     (null fill-pointer)
	     (null displaced-to)
	     (null displaced-index-offset))
	(multiple-value-bind (element-type-tag element-size default-initial)
	    (type->element-type-tag (eval element-type))
	  ;; HEY! Add check for constant dims here
	  (let ((ie (if ie? initial-element default-initial)))
	    `(make-simple-array ,dims ,element-type-tag ,element-size
	      ,ie ,initial-contents)))
	orig))

  (define-compiler-macro aref (&whole orig array &rest indices)
    (if (= (length indices) 1)
	`(vref ,array ,(first indices))
	orig))

  (define-compiler-macro set-aref (&whole orig value array &rest indices)
    (if (= (length indices) 1)
	`(set_vref ,value ,array ,(first indices))
	orig))

  (define-compiler-macro schar (&whole orig string index)
    (if (config-array-bounds-checking? *config*)
	orig	
	`(%schar ,string ,index)))

  (define-compiler-macro set-schar (&whole orig string index value)
    (if (config-array-bounds-checking? *config*)
	orig
	(let ((v (gensym "V")))
	  `(let ((,v ,value))
	    (%set-schar ,string ,index ,v)
	    ,v))))

  (define-compiler-macro svref (&whole orig vector index)
    (if (config-array-bounds-checking? *config*)
	orig
	`(%32bit-ref ,vector ,index)))

  (define-compiler-macro set-svref (&whole orig vector index value)
    (if (config-array-bounds-checking? *config*)
	orig
	(let ((v (gensym "V")))
	  `(let ((,v ,value))
	    (%32bit-def ,vector ,index ,v)
	    ,v))))

  (define-compiler-macro list (&whole orig &rest args)
    (case (length args)
      (0 nil)
      (1 `(cons ,(first args) nil))
      (2 `(cons ,(first args) (cons ,(second args) nil)))
      (3 `(cons ,(first args) (cons ,(second args) (cons ,(third args) nil))))
      (t orig)))


  ;; Don't need this once we can do beta subst AFTER meta-eval. 
  (define-compiler-macro ldb (&whole orig byte-spec n)
    (if (and (listp byte-spec) (eq (car byte-spec) 'byte))
	(destructuring-bind (size position) (cdr byte-spec)
	  `(ldb-1 ,size ,position ,n))
	orig))
  )


(defun initialize-compiler-methods ()
  (define-compiler-method (add %f+) ((x float) (y float) => (r t)))
  (define-compiler-method (subtract %f-) ((x float) (y float) => (r t)))
  (define-compiler-method (multiply %f*) ((x float) (y float) => (r t)))
  (define-compiler-method (divide %f/) ((x float) (y float) => (r t)))

  (define-compiler-method (num_equal_p %f=) ((x float) (y float) => (r t)))
  (define-compiler-method (greaterp %f>) ((x float) (y float) => (result t)))
  (define-compiler-method (lessp %f<) ((x float) (y float) => (result t)))
  (define-compiler-method (leq_p %f<=) ((x float) (y float) => (result t)))
  (define-compiler-method (geq_p %f>=) ((x float) (y float) => (result t)))

  (define-compiler-method (add %+) ((x fixnum) (y fixnum) => (r fixnum)))
  (define-compiler-method (subtract %-) ((x fixnum) (y fixnum) => (r fixnum)))
  (define-compiler-method (multiply %*) ((x fixnum) (y fixnum) => (r fixnum)))
  (define-compiler-method (rem %rem) ((x fixnum) (y fixnum) => (r t)))
  (define-compiler-method (floor/1v %div) ((x fixnum) (y fixnum) => (r t)))

  (define-compiler-method (num_equal_p %=) ((x fixnum) (y fixnum) => (r t)))

  (define-compiler-method (c_eql %=) ((x (or fixnum character)) (y t)
				      => (r t)))
  (define-compiler-method (c_eql %=) ((x t) (y (or fixnum character))
				      => (r t)))

  (define-compiler-method (greaterp %>) ((x fixnum) (y fixnum) => (result t)))
  (define-compiler-method (lessp %<) ((x fixnum) (y fixnum) => (result t)))
  (define-compiler-method (leq_p %<=) ((x fixnum) (y fixnum) => (result t)))
  (define-compiler-method (geq_p %>=) ((x fixnum) (y fixnum) => (result t)))

  (define-compiler-method (logand/2 %logand)
      ((x fixnum) (y fixnum) => (r t)))

  (define-compiler-method (logior/2 %logior)
      ((x fixnum) (y fixnum) => (r t)))

  (define-compiler-method (logxor/2 %logxor)
      ((x fixnum) (y fixnum) => (r t)))

  (define-compiler-method (length %object-length)
      ((s (simple-array * (*))) => (result t)))

  (define-compiler-method (ldb-1 %load-field)
      ((size fixnum) (position fixnum) (n fixnum) => (field t))
    ;; This is a terrible hack until other parts of the compiler are smarter.
    #'(lambda (tree)
	(flet ((field-mask (size)
		 (- (ash 1 size) 1)))
	  (let ((args (function-call-args tree)))
	    (when (constant-p (first args))
	      (setf (constant-data (first args))
		    (field-mask (constant-data (first args)))))))))

  (define-compiler-method (logbitp %logbitp)
      ((index fixnum) (n fixnum) => (flag t)))

  (define-meta-eval (add +) (number number))
  (define-meta-eval (subtract -) (number number))
  (define-meta-eval (multiply *) (number number))

  (define-meta-eval byte-size (byte-specifier))
  (define-meta-eval byte-position (byte-specifier))
  (define-meta-eval byte (fixnum fixnum))
  )

(defun initialize-primitives ()
  (def-integer-arith-op/2 %+ "+")
  (def-integer-arith-op/2 %- "-")
  (def-integer-arith-op/2 %logand "&")
  (def-integer-arith-op/2 %logior "|")
  (def-integer-arith-op/2 %logxor "^")
  (def-integer-arith-predicate/2 %= "==")
  (def-integer-arith-predicate/2 %< "<")
  (def-integer-arith-predicate/2 %<= "<=")
  (def-integer-arith-predicate/2 %> ">")
  (def-integer-arith-predicate/2 %>= ">=")

  (def-float-arith-op/2 %f+ "+")
  (def-float-arith-op/2 %f- "-")
  (def-float-arith-op/2 %f* "*")
  (def-float-arith-op/2 %f/ "/")
  (def-float-arith-predicate/2 %f= "==")
  (def-float-arith-predicate/2 %f< "<")
  (def-float-arith-predicate/2 %f<= "<=")
  (def-float-arith-predicate/2 %f> ">")
  (def-float-arith-predicate/2 %f>= ">=")

  (defprimitive %* ((x t) (y t) => (result t))
    (emit-c "(LP) ((long) ~A * FX_TO_LONG(~A))" x y)) ; pre-adjust

  (defprimitive %div ((x t) (y t) => (result t))
    (emit-c "(LP) LONG_TO_FX((long) ~A / (long) ~A)" x y)) ; post-adjust

  (defprimitive %rem ((x t) (y t) => (result t))
    (emit-c "(LP) ((long) ~A % (long) ~A)" x y))

  (defprimitive %fixnump ((x t) => (flag if-test))
    (emit-c "FIXNUMP(~A)" x))

  (defprimitive %tag ((x t) => (tag-byte int))
    (emit-c "((long) TAG(~A))" x))

  (defprimitive %tag_mask ((x t) (mask int) => (tag-byte int))
    (emit-c "((long) TAG(~A)) & ~D" x mask))

  (defprimitive %tag= ((x t) (y int) => (flag if-test))
    (emit-c "OTHER_PTRP(~A) && (TAG(~A) == ~A)" x x y))

  (defprimitive %tag_mask= ((x t) (mask int) (y int) => (flag if-test))
    (emit-c "OTHER_PTRP(~A) && ((HEADER(~A) & ~D) == ~A)"
	    x x mask y))

  (defprimitive %numberp ((x t) => (flag if-test))
    (emit-c "(FIXNUMP(~A) || (((HEADER(~A)) & 0x3) == 0x1))"
	    x x))

  (defprimitive %eq ((x t) (y t) => (flag if-test))
    (emit-c "(~A == ~A)" x y))

  (defprimitive %pointer ((x t) => (pointer uint32))
    (emit-c "((long) ~A)" x))

  (defprimitive %lognot ((x int) => (result int))
    (emit-c "((long) ~~(~A))" x))

  (defprimitive %ashl ((x int) (count int) => (result int))
    (emit-c "((unsigned long) (~A) << ~D)" x count))

  (defprimitive %ashr ((x int) (count int) => (result int))
    (emit-c "((unsigned long) (~A) >> (~D))" x count))

  (defprimitive %set-object-length ((x t) (len int) => ())
    (emit-c "(LP) ((LHEADER(~A) = TAG(~A) + (~A << 8)))" x x len))

  (defprimitive %object-length ((x t) => (len int))
    (emit-c "(LP) (DEREF(~A - sizeof(long)) >> 8)" x))

  (defprimitive %structure-elt ((s t) (i int) => (v t))
    (emit-c "(LP) DEREF(~A + ~A * sizeof(LP))" s i))

  (defprimitive %set-structure-elt ((s t) (i int) (value t) => ())
    (emit-c "(LP) (DEREF(~A + ~A * sizeof(LP)) = (LD) ~A)" s i value))

  ;; New accessors
  
  (defprimitive %unsigned-8bit-ref ((x t) (i int) => (v int))
    (emit-c "(LP) (*(((unsigned char *) ((long) ~A - 1)) + ~A))" x i))

  (defprimitive %unsigned-16bit-ref ((x t) (i int) => (v int))
    (emit-c "(LP) (*(((unsigned short *) ((long) ~A - 1)) + ~A))" x i))

  (defprimitive %unsigned-31bit-ref ((x t) (i int) => (v uint31))
    (emit-c "(LP) (*(((unsigned long *) ((long) ~A - 1)) + ~A))" x i))

  (defprimitive %unsigned-32bit-ref ((x t) (i int) => (v uint32))
    (emit-c "(LP) (*(((unsigned long *) ((long) ~A - 1)) + ~A))" x i))
  
  (defprimitive %signed-8bit-ref ((x t) (i int) => (v int))
    (emit-c "(LP) (*(((char *) ((long) ~A - 1)) + ~A))" x i))

  (defprimitive %signed-16bit-ref ((x t) (i int) => (v int))
    (emit-c "(LP) (*(((short *) ((long) ~A - 1)) + ~A))" x i))

  (defprimitive %signed-32bit-ref ((x t) (i int) => (v int32))
    (emit-c "(LP) (*(((long *) ((long) ~A - 1)) + ~A))" x i))


  (defprimitive %unsigned-8bit-def ((x t) (i int) (v int) => ())
    (emit-c
     "(LP) ((*(((unsigned char *) ((long) ~A - 1)) + ~A)) = (unsigned char) ~A)"
     x i v))

  (defprimitive %unsigned-16bit-def ((x t) (i int) (v int) => ())
    (emit-c
     "(LP) ((*(((unsigned short *) ((long)~A - 1))+~A)) = (unsigned short) ~A)"
     x i v))

  (defprimitive %unsigned-31bit-def ((x t) (i int) (v uint31) => ())
    (emit-c
     "(LP) ((*(((unsigned long *) ((long) ~A - 1)) + ~A)) = (unsigned long) ~A)"
     x i v))

  (defprimitive %unsigned-32bit-def ((x t) (i int) (v uint32) => ())
    (emit-c
     "(LP) ((*(((unsigned long *) ((long) ~A - 1)) + ~A)) = (unsigned long) ~A)"
     x i v))
  
  (defprimitive %signed-8bit-def ((x t) (i int) (v int) => ())
    (emit-c "(LP) ((*(((char *) ((long) ~A - 1)) + ~A)) = (char) ~A)" x i v))

  (defprimitive %signed-16bit-def ((x t) (i int) (v int) => ())
    (emit-c "(LP) ((*(((short *) ((long) ~A - 1)) + ~A)) = (short) ~A)" x i v))

  (defprimitive %signed-32bit-def ((x t) (i int) (v int32) => ())
    (emit-c "(LP) ((*(((long *) ((long) ~A - 1)) + ~A)) = (long) ~A)" x i v))


  ;; HEY! these 2 should be called %simple-vector-ref and def
  (defprimitive %32bit-ref ((x t) (i int) => (v t)) ; unsigned
    (emit-c "(LP) DEREF(~A + ~A * sizeof(LP))" x i))

  (defprimitive %32bit-def ((x t) (i int) (y t) => ()) ; unsigned
    (emit-c "(LP) (DEREF(~A + ~A * sizeof(LP)) = (LD) ~A)" x i y))

;  (defprimitive %signed-32bit-ref ((x t) (i int) => (v int))
;   (emit-c "(LP) DEREF(~A + ~A * sizeof(LP))" x i))

;  (defprimitive %signed-32bit-def ((x t) (i int) (y int) => ())
;   (emit-c "(LP) (DEREF(~A + ~A * sizeof(LP)) = (LD) ~A)" x i y))

  (defprimitive %schar ((x t) (i int) => (v char))
    (emit-c "(LP) *(~A - 1 + ~A)" x i))

  (defprimitive %set-schar ((x t) (i int) (y char) => ())
    (emit-c "(LP) (*(~A - 1 + ~A) = (char) ~A)" x i y))

  (defprimitive %code-char ((x int) => (c char))
    (emit-c "((unsigned char) ~A)" x))

  (defprimitive %char-code ((c char) => (x int))
    (emit-c "((long) ~A)" c))

  (defprimitive %int->double ((i int) => (d double))
    (emit-c "((double) ~A)" i))

  (defprimitive %bit-test ((start t) (word int) (bit int)
			   => (value int))
    (emit-c "(((unsigned long) (DEREF(~A + ~A * sizeof(LP))) >> ~A) & 1)"
	    start word bit))

  ;; HEY! unify with %bit-test?
  (defprimitive %logbitp ((index int) (n int) => (flag if-test))
    (emit-c " ((((unsigned long) ~A) >> ~D) & 1)" n index))

  (defprimitive %word-ptr ((start t) (word int) => (ptr t))
    (emit-c "(~A + ~A * sizeof(LP))" start word))

  (defprimitive %bit-set ((word-ptr t) (bit int) => ())
    (emit-c "(LP) (DEREF(~A) = ((1 << ~A) | DEREF(~A)))"
	    word-ptr bit word-ptr))

  (defprimitive %bit-clear ((word-ptr t) (bit int) => ())
    (emit-c "(LP) (DEREF(~A) = ((~~(1 << ~A)) & DEREF(~A)))"
	    word-ptr bit word-ptr))

  (defprimitive %field-mask ((size int) => (mask int))
    (emit-c "((1 << ~D) - 1)" size))

  (defprimitive %load-field ((mask int) (position int) (n int) => (field int))
    (emit-c "(((long) ~A >> (long) ~A ) & (long) ~A)"
	    n position mask))

;;; HEY! add %store-field here!


;;; ******* Normal versions of symbol primitives.
;;; Some library things still use symbol-self-link-offset directly!

  ;; HEY! This one doesn't indirect through the link field.
  (defprimitive %boundp ((sym t) => (flag if-test))
    (emit-c "(LDREF(~A,SYMBOL,value) != UBV_MARKER)" sym))

  ;; HEY! Check that the fcell really contains a function, or
  ;; rely on setters to always check this?
  (defprimitive %fboundp ((sym t) => (flag if-test))
    (emit-c "(LDREF(~A,SYMBOL,function) != (LP) LREF(ubf_procedure))" sym))

  (defprimitive %makunbound ((sym t) => ())
    (emit-c "LDREF(~A,SYMBOL,value) = UBV_MARKER" sym))

  (defprimitive %fmakunbound ((sym t) => ())
    (emit-c "(LDREF(~A,SYMBOL,function) = (LP) LREF(ubf_procedure))" sym))

  (defprimitive %symref ((sym t) (i int) => (v t))
    (emit-c "(LP) DEREF(~A + ~A * sizeof(LP))" sym i))

  (defprimitive %symdef ((sym t) (i int) (y t) => ())
    (emit-c "(LP) (DEREF(~A + ~A * sizeof(LP)) = (LD) ~A)"
	    sym i y))
  
  )


(defun initialize-function-methods ()
  ;; Moved to library
  ;; (initialize-foreign-info)
  (initialize-compiler-macros)
  (initialize-compiler-methods)
  (initialize-primitives))



