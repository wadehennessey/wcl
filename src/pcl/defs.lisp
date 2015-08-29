;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;

(in-package 'pcl)


;;;
;;; Convert a function name to its standard setf function name.  We have to
;;; do this hack because not all Common Lisps have yet converted to having
;;; setf function specs.
;;;
;;; In a port that does have setf function specs you can use those just by
;;; making the obvious simple changes to these functions.  The rest of PCL
;;; believes that there are function names like (SETF <foo>), this is the
;;; only place that knows about this hack.
;;;
(eval-when (compile load eval)

(defvar *setf-function-names* (make-hash-table :size 200 :test #'eq))

(defun get-setf-function-name (name)
  (or (gethash name *setf-function-names*)
      (setf (gethash name *setf-function-names*)
	    (intern (format nil
			    "SETF ~A ~A"
			    (package-name (symbol-package name))
			    (symbol-name name))
		    *the-pcl-package*))))

;;;
;;; Call this to define a setf macro for a function with the same behavior as
;;; specified by the SETF function cleanup proposal.  Specifically, this will
;;; cause: (SETF (FOO a b) x) to expand to (|SETF FOO| x a b).
;;;
;;; do-standard-defsetf                  A macro interface for use at top level
;;;                                      in files.  Unfortunately, users may
;;;                                      have to use this for a while.
;;;                                      
;;; do-standard-defsetfs-for-defclass    A special version called by defclass.
;;; 
;;; do-standard-defsetf-1                A functional interface called by the
;;;                                      above, defmethod and defgeneric.
;;;                                      Since this is all a crock anyways,
;;;                                      users are free to call this as well.
;;;
(defmacro do-standard-defsetf (&rest function-names)
  `(eval-when (compile load eval)
     (dolist (fn-name ',function-names) (do-standard-defsetf-1 fn-name))))

(defun do-standard-defsetfs-for-defclass (accessors)
  (dolist (name accessors) (do-standard-defsetf-1 name)))

(defun do-standard-defsetf-1 (function-name)
  (unless (setfboundp function-name)
    (let* ((setf-function-name (get-setf-function-name function-name)))
    
      #+Genera
      (let ((fn #'(lambda (form)
		    (lt::help-defsetf
		      '(&rest accessor-args) '(new-value) function-name 'nil
		      `(`(,',setf-function-name ,new-value .,accessor-args))
		      form))))
	(setf (get function-name 'lt::setf-method) fn
	      (get function-name 'lt::setf-method-internal) fn))

      #+Lucid
      (lucid::set-simple-setf-method 
	function-name
	#'(lambda (form new-value)
	    (let* ((bindings (mapcar #'(lambda (x) `(,(gensym) ,x))
				     (cdr form)))
		   (vars (mapcar #'car bindings)))
	      ;; This may wrap spurious LET bindings around some form,
	      ;;   but the PQC compiler will unwrap then.
	      `(LET (,.bindings)
		 (,setf-function-name ,new-value . ,vars)))))
      
      #+kcl
      (let ((helper (gensym)))
	(setf (macro-function helper)
	      #'(lambda (form env)
		  (declare (ignore env))
		  (let* ((loc-args (butlast (cdr form)))
			 (bindings (mapcar #'(lambda (x) `(,(gensym) ,x)) loc-args))
			 (vars (mapcar #'car bindings)))
		    `(let ,bindings
		       (,setf-function-name ,(car (last form)) ,@vars)))))
	(eval `(defsetf ,function-name ,helper)))
      #+Xerox
      (flet ((setf-expander (body env)
	       (declare (ignore env))
	       (let ((temps
		       (mapcar #'(lambda (x) (declare (ignore x)) (gensym))
			       (cdr body)))
		     (forms (cdr body))
		     (vars (list (gensym))))
		 (values temps
			 forms
			 vars
			 `(,setf-function-name ,@vars ,@temps)
			 `(,function-name ,@temps)))))
	(let ((setf-method-expander (intern (concatenate 'string
						         (symbol-name function-name)
						         "-setf-expander")
				     (symbol-package function-name))))
	  (setf (get function-name :setf-method-expander) setf-method-expander
		(symbol-function setf-method-expander) #'setf-expander)))
      
      #+wcl
      (lisp::define-setf function-name
	  #'(lambda (access env)
	      (let ((svar (gensym "S"))
		    (tvars (loop for arg in (cdr access)
				 collect (gensym "T"))))
		(values tvars
			(cdr access)
			(list svar)
			(apply #'(lambda (new-value &rest accessor-args)
				   `(,setf-function-name
				     ,new-value ,@accessor-args))
			       svar
			       tvars)
			`(,(first access) ,@tvars)))))

      #-(or Genera Lucid kcl Xerox wcl)
      (eval `(defsetf ,function-name (&rest accessor-args) (new-value)
	       `(,',setf-function-name ,new-value ,@accessor-args)))
      
      )))

(defun setfboundp (symbol)
  #+Genera (not (null (get-properties (symbol-plist symbol)
				      'lt::(derived-setf-function trivial-setf-method
					    setf-equivalence setf-method))))
  #+Lucid  (locally
	     (declare (special lucid::*setf-inverse-table*
			       lucid::*simple-setf-method-table*
			       lucid::*setf-method-expander-table*))
	     (or (gethash symbol lucid::*setf-inverse-table*)
		 (gethash symbol lucid::*simple-setf-method-table*)
		 (gethash symbol lucid::*setf-method-expander-table*)))
  #+kcl    (or (get symbol 'si::setf-method)
	       (get symbol 'si::setf-update-fn)
	       (get symbol 'si::setf-lambda))
  #+Xerox  (or (get symbol :setf-inverse)
	       (get symbol 'il:setf-inverse)
	       (get symbol 'il:setfn)
	       (get symbol :shared-setf-inverse)
	       (get symbol :setf-method-expander)
	       (get symbol 'il:setf-method-expander))

  #+:coral (or (get symbol 'ccl::setf-inverse)
	       (get symbol 'ccl::setf-method-expander))

  #+WCL    nil ; (gethash symbol lisp::*setf-methods*)

  #-(or Genera Lucid KCL Xerox :coral WCL) nil)

);eval-when


;;;
;;; PCL, like user code, must endure the fact that we don't have a properly
;;; working setf.  Many things work because they get mentioned by a defclass
;;; or defmethod before they are used, but others have to be done by hand.
;;; 
(do-standard-defsetf
  class-wrapper					;***
  generic-function-name
  method-function-plist
  method-function-get
  gdefinition
  slot-value-using-class
  )

(defsetf slot-value set-slot-value)


;;;
;;; This is like fdefinition on the Lispm.  If Common Lisp had something like
;;; function specs I wouldn't need this.  On the other hand, I don't like the
;;; way this really works so maybe function specs aren't really right either?
;;; 
;;; I also don't understand the real implications of a Lisp-1 on this sort of
;;; thing.  Certainly some of the lossage in all of this is because these
;;; SPECs name global definitions.
;;;
;;; Note that this implementation is set up so that an implementation which
;;; has a 'real' function spec mechanism can use that instead and in that way
;;; get rid of setf generic function names.
;;;
(defmacro parse-gspec (spec
		       (non-setf-var . non-setf-case)
		       (setf-var . setf-case))
  (declare (indentation 1 1))
  (once-only (spec)
    `(cond ((symbolp ,spec)
	    (let ((,non-setf-var ,spec)) ,@non-setf-case))
	   ((and (listp ,spec)
		 (eq (car ,spec) 'setf)
		 (symbolp (cadr ,spec)))
	    (let ((,setf-var (cadr ,spec))) ,@setf-case))
	   (t
	    (error
	      "Can't understand ~S as a generic function specifier.~%~
               It must be either a symbol which can name a function or~%~
               a list like ~S, where the car is the symbol ~S and the cadr~%~
               is a symbol which can name a generic function."
	      ,spec '(setf <foo>) 'setf)))))

;;;
;;; If symbol names a function which is traced or advised, return the
;;; unadvised, traced etc. definition.  This lets me get at the generic
;;; function object even when it is traced.
;;;
(defun unencapsulated-fdefinition (symbol)
  #+Lispm (si:fdefinition (si:unencapsulate-function-spec symbol))
  #+Lucid (lucid::get-unadvised-procedure (symbol-function symbol))
  #+excl  (or (excl::encapsulated-basic-definition symbol)
	      (symbol-function symbol))
  #+xerox (il:virginfn symbol)
  
  #-(or Lispm Lucid excl Xerox) (symbol-function symbol))

;;;
;;; If symbol names a function which is traced or advised, redefine
;;; the `real' definition without affecting the advise.
;;;
(defun fdefine-carefully (symbol new-definition)
  #+Lispm (si:fdefine symbol new-definition t t)
  #+Lucid (let ((lucid::*redefinition-action* nil))
	    (setf (symbol-function symbol) new-definition))
  #+excl  (setf (symbol-function symbol) new-definition)
  #+xerox (let ((advisedp (member symbol il:advisedfns :test #'eq))
                (brokenp (member symbol il:brokenfns :test #'eq)))
	    ;; In XeroxLisp (late of envos) tracing is implemented
	    ;; as a special case of "breaking".  Advising, however,
	    ;; is treated specially.
            (xcl:unadvise-function symbol :no-error t)
            (xcl:unbreak-function symbol :no-error t)
            (setf (symbol-function symbol) new-definition)
            (when brokenp (xcl:rebreak-function symbol))
            (when advisedp (xcl:readvise-function symbol)))

  #-(or Lispm Lucid excl Xerox)
  (setf (symbol-function symbol) new-definition)
  
  new-definition)

(defun gboundp (spec)
  (parse-gspec spec
    (name (fboundp name))
    (name (fboundp (get-setf-function-name name)))))

(defun gmakunbound (spec)
  (parse-gspec spec
    (name (fmakunbound name))
    (name (fmakunbound (get-setf-function-name name)))))

(defun gdefinition (spec)
  (parse-gspec spec
    (name (or (macro-function name)		;??
	      (unencapsulated-fdefinition name)))
    (name (unencapsulated-fdefinition (get-setf-function-name name)))))

(defun SETF\ PCL\ GDEFINITION (new-value spec)
  (parse-gspec spec
    (name (fdefine-carefully name new-value))
    (name (fdefine-carefully (get-setf-function-name name) new-value))))


(defun type-class (type)
  (if (consp type)
      (case (car type)
	(class-eq (cadr type))
	(eql (class-of (cadr type)))
	(t (and (null (cdr type)) (find-class (car type) nil))))
      (if (symbolp type)
	  (find-class type nil)
	  (and (class-specializer-p type)
	       (specializer-class type)))))

(defun class-type-p (type)
  (if (consp type)
      (and (null (cdr type)) (find-class (car type) nil))
      (if (symbolp type)
	  (find-class type nil)
	  (and (classp type) type))))
;;;;;;
(defun exact-class-type-p (type)
  (if (consp type)
      (or (eq (car type) 'class-eq) (eq (car type) 'eql))
      (exact-class-specializer-p type)))

(defun make-class-eq-predicate (class)
  (when (symbolp class) (setq class (find-class class)))
  #'(lambda (object) (eq class (class-of object))))

(deftype class-eq (class)
  `(satisfies ,(make-class-eq-predicate class)))

(defun class-eq-type-p (type)
  (if (consp type)
      (eq (car type) 'class-eq)
      (class-eq-specializer-p type)))
;;;;;;
(defun make-eql-predicate (eql-object)
  #'(lambda (object) (eql eql-object object)))

(deftype eql (type-object)
  `(satisfies ,(make-eql-predicate type-object)))

(defun eql-type-p (type)
  (if (consp type)
      (eq (car type) 'eql)
      (eql-specializer-p type)))

(defun type-object (type)
  (if (consp type)
      (cadr type)
      (specializer-object type)))

;;;;;;
(defun not-type-p (type)
  (and (consp type) (eq (car type) 'not)))

(defun not-type (type)
  (cadr type))

;;;
;;; These functions are a pale imitiation of their namesake.  They accept
;;; class objects or types where they should.
;;; 
(defun *typep (object type)
  (let ((specializer (or (class-type-p type) 
			 (and (specializerp type) type))))
    (cond (specializer
	    (specializer-type-p object specializer))
	  ((not-type-p type)
	   (not (*typep object (not-type type))))
	  (t
	   (typep object type)))))

(defun *subtypep (type1 type2)
  (let ((c1 (class-type-p type1))
	(c2 (class-type-p type2)))
    (cond ((and c1 c2)
	   (values (memq c2 (class-precedence-list c1)) t))
	  ((setq c1 (or c1 (specializerp type1)))
	   (specializer-applicable-using-type-p c1 type2))
	  ((or (null c2) (classp c2))
	   (subtypep type1 (if c2 (class-name c2) type2))))))

(defun do-satisfies-deftype (name predicate)
  (let* ((specifier `(satisfies ,predicate))
	 (expand-fn #'(lambda (&rest ignore)
			(declare (ignore ignore))
			specifier)))
    ;; Specific ports can insert their own way of doing this.  Many
    ;; ports may find the expand-fn defined above useful.
    ;;
    (or #+:Genera
	(setf (get name 'deftype) expand-fn)
	#+(and :Lucid (not :Prime))
	(system::define-macro `(deftype ,name) expand-fn nil)
	#+ExCL
	(setf (get name 'excl::deftype-expander) expand-fn)
	#+:coral
	(setf (get name 'ccl::deftype-expander) expand-fn)

	;; This is the default for ports for which we don't know any
	;; better.  Note that for most ports, providing this definition
	;; should just speed up class definition.  It shouldn't have an
	;; effect on performance of most user code.
	(eval `(deftype ,name () '(satisfies ,predicate))))))

(defun make-type-predicate-name (name)
  (intern (format nil
		  "TYPE-PREDICATE ~A ~A"
		  (package-name (symbol-package name))
		  (symbol-name name))
	  *the-pcl-package*))



(proclaim '(special *the-class-t* 
		    *the-class-vector* *the-class-symbol*
		    *the-class-string* *the-class-sequence*
		    *the-class-rational* *the-class-ratio*
		    *the-class-number* *the-class-null* *the-class-list*
		    *the-class-integer* *the-class-float* *the-class-cons*
		    *the-class-complex* *the-class-character*
		    *the-class-bit-vector* *the-class-array*

		    *the-class-standard-object*
		    *the-class-class*
		    *the-class-method*
		    *the-class-generic-function*
		    *the-class-standard-class*
	            *the-class-funcallable-standard-class*
		    *the-class-standard-method*
		    *the-class-standard-generic-function*
	            *the-class-standard-effective-slot-definition*

	            *the-eslotd-standard-class-slots*
	            *the-eslotd-funcallable-standard-class-slots*))

(proclaim '(special *the-wrapper-of-t*
		    *the-wrapper-of-vector* *the-wrapper-of-symbol*
		    *the-wrapper-of-string* *the-wrapper-of-sequence*
		    *the-wrapper-of-rational* *the-wrapper-of-ratio*
		    *the-wrapper-of-number* *the-wrapper-of-null*
		    *the-wrapper-of-list* *the-wrapper-of-integer*
		    *the-wrapper-of-float* *the-wrapper-of-cons*
		    *the-wrapper-of-complex* *the-wrapper-of-character*
		    *the-wrapper-of-bit-vector* *the-wrapper-of-array*))



(defvar *built-in-class-symbols* ())
(defvar *built-in-wrapper-symbols* ())

(defun get-built-in-class-symbol (class-name)
  (or (cadr (assq class-name *built-in-class-symbols*))
      (let ((symbol (intern (format nil
				    "*THE-CLASS-~A*"
				    (symbol-name class-name))
			    *the-pcl-package*)))
	(push (list class-name symbol) *built-in-class-symbols*)
	symbol)))

(defun get-built-in-wrapper-symbol (class-name)
  (or (cadr (assq class-name *built-in-wrapper-symbols*))
      (let ((symbol (intern (format nil
				    "*THE-WRAPPER-OF-~A*"
				    (symbol-name class-name))
			    *the-pcl-package*)))
	(push (list class-name symbol) *built-in-wrapper-symbols*)
	symbol)))




(pushnew 'class *variable-declarations*)
(pushnew 'variable-rebinding *variable-declarations*)

(defun variable-class (var env)
  (caddr (variable-declaration 'class var env)))


(defvar *boot-state* ())			;NIL
						;EARLY
						;BRAID
						;COMPLETE

(eval-when (load eval)
  (when (eq *boot-state* 'complete)
    (error "Trying to load (or compile) PCL in an environment in which it~%~
            has already been loaded.  This doesn't work, you will have to~%~
            get a fresh lisp (reboot) and then load PCL."))
  (when *boot-state*
    (cerror "Try loading (or compiling) PCL anyways."
	    "Trying to load (or compile) PCL in an environment in which it~%~
             has already been partially loaded.  This may not work, you may~%~
             need to get a fresh lisp (reboot) and then load PCL."))
  )

;;;
;;; This is used by combined methods to communicate the next methods to
;;; the methods they call.  This variable is captured by a lexical variable
;;; of the methods to give it the proper lexical scope.
;;; 
(defvar *next-methods* nil)

(defvar *not-an-eql-specializer* '(not-an-eql-specializer))

(defvar *umi-gfs*)
(defvar *umi-complete-classes*)
(defvar *umi-reorder*)

(defvar *invalidate-discriminating-function-force-p* ())
(defvar *invalid-dfuns-on-stack* ())


(defvar *standard-method-combination*)

(defvar *slotd-unsupplied* (list '*slotd-unsupplied*))	;***


(defmacro define-gf-predicate (predicate &rest classes)
  `(progn (defmethod ,predicate ((x t)) nil)
	  ,@(mapcar #'(lambda (c) `(defmethod ,predicate ((x ,c)) t))
		    classes)))

(defmacro plist-value (object name)
  `(with-slots (plist) ,object (getf plist ,name)))

(defsetf plist-value (object name) (new-value)
  (once-only (new-value)
    `(with-slots (plist) ,object
       (if ,new-value
	   (setf (getf plist ,name) ,new-value)
	   (progn (remf plist ,name) nil)))))



(defvar *built-in-classes*
  ;;
  ;; name       supers     subs                     cdr of cpl
  ;;
  '((number     (t)        (complex float rational) (t))
    (complex    (number)   ()                       (number t))
    (float      (number)   ()                       (number t))
    (rational   (number)   (integer ratio)          (number t))
    (integer    (rational) ()                       (rational number t))
    (ratio      (rational) ()                       (rational number t))

    (sequence   (t)        (list vector)            (t))
    (list       (sequence) (cons null)              (sequence t))
    (cons       (list)     ()                       (list sequence t))
    

    (array      (t)        (vector)                 (t))
    (vector     (array
		 sequence) (string bit-vector)      (array sequence t))
    (string     (vector)   ()                       (vector array sequence t))
    (bit-vector (vector)   ()                       (vector array sequence t))
    (character  (t)        ()                       (t))
   
    (symbol     (t)        (null)                   (t))
    (null       (symbol)   ()                       (symbol list sequence t))))


;;;
;;; The classes that define the kernel of the metabraid.
;;;
(defclass t () ()
  (:metaclass built-in-class))

(defclass standard-object (t) ())

(defclass metaobject (standard-object) ())

(defclass specializer (metaobject) ())

(defclass class-specializer (specializer) ())

(defclass definition-source-mixin (standard-object)
     ((source
	:initform (load-truename)
	:reader definition-source
	:initarg :definition-source)))

(defclass plist-mixin (standard-object)
     ((plist
	:initform ())))

(defclass documentation-mixin (plist-mixin)
     ())

(defclass dependent-update-mixin (plist-mixin)
    ())

;;;
;;; The class CLASS is a specified basic class.  It is the common superclass
;;; of any kind of class.  That is any class that can be a metaclass must
;;; have the class CLASS in its class precedence list.
;;; 
(defclass class (documentation-mixin dependent-update-mixin definition-source-mixin
				     class-specializer)
     ((name
	:initform nil
	:initarg  :name
	:accessor class-name)
      (direct-superclasses
	:initform ()
	:reader class-direct-superclasses)
      (direct-subclasses
	:initform ()
	:reader class-direct-subclasses)
      (direct-methods
	:initform (cons nil nil))))

;;;
;;; The class PCL-CLASS is an implementation-specific common superclass of
;;; all specified subclasses of the class CLASS.
;;; 
(defclass pcl-class (class)
     ((class-precedence-list
	:initform ())
      (wrapper
	:initform nil)))

;;;
;;; The class STD-CLASS is an implementation-specific common superclass of
;;; the classes STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.
;;; 
(defclass std-class (pcl-class)
     ((direct-slots
	:initform ()
	:accessor class-direct-slots)
      (slots
	:initform ()
	:accessor class-slots)
      (no-of-instance-slots		    ;*** MOVE TO WRAPPER ***
	:initform 0
	:accessor class-no-of-instance-slots)
      (prototype
	:initform nil)))

(defclass standard-class (std-class)
     ())

(defclass funcallable-standard-class (std-class)
     ())
    
(defclass forward-referenced-class (pcl-class) ())

(defclass built-in-class (pcl-class) ())


;;;
;;; Slot definitions.
;;;
;;; Note that throughout PCL, "SLOT-DEFINITION" is abbreviated as "SLOTD".
;;;
(defclass slot-definition (metaobject) ())

(defclass direct-slot-definition    (slot-definition) ())
(defclass effective-slot-definition (slot-definition) ())

(defclass standard-slot-definition (slot-definition) 
     ((name
	:initform nil
        :accessor slotd-name)
      (initform
	:initform *slotd-unsupplied*
	:accessor slotd-initform)
      (initfunction
	:initform *slotd-unsupplied*
	:accessor slotd-initfunction)
      (readers
	:initform nil
	:accessor slotd-readers)
      (writers
	:initform nil
	:accessor slotd-writers)
      (initargs
	:initform nil
	:accessor slotd-initargs)
      (allocation
	:initform nil
	:accessor slotd-allocation)
      (type
	:initform nil
	:accessor slotd-type)
      (documentation
	:initform ""
	:initarg :documentation)
      (class
        :initform nil
	:accessor slotd-class)
      (instance-index
        :initform nil
	:accessor slotd-instance-index)))

(defclass standard-direct-slot-definition (standard-slot-definition
					   direct-slot-definition)
     ())					;Adding slots here may
						;involve extra work to
						;the code in braid.lisp

(defclass standard-effective-slot-definition (standard-slot-definition
					      effective-slot-definition)
     ())					;Adding slots here may
						;involve extra work to
						;the code in braid.lisp



(defclass eql-specializer (specializer)
     ((object :initarg :object :reader specializer-object)))



;;;
;;;
;;;
(defmacro dolist-carefully ((var list improper-list-handler) &body body)
  `(let ((,var nil)
	 (.dolist-carefully. ,list))
     (loop (when (null .dolist-carefully.) (return nil))
	   (if (consp .dolist-carefully.)
	       (progn
		 (setq ,var (pop .dolist-carefully.))
		 ,@body)
	       (,improper-list-handler)))))


