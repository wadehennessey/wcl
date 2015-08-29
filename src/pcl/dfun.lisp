;;; -*- Mode:LISP; Package:PCL; Base:10; Syntax:Common-Lisp -*-
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

#|

This implementation of method lookup was redone in early August of 89.

It has the following properties:

 - It's modularity makes it easy to modify the actual caching algorithm.
   The caching algorithm is almost completely separated into the files
   cache.lisp and dlap.lisp.  This file just contains the various uses
   of it. There will be more tuning as we get more results from Luis'
   measurements of caching behavior.

 - The metacircularity issues have been dealt with properly.  All of
   PCL now grounds out properly.  Moreover, it is now possible to have
   metaobject classes which are themselves not instances of standard
   metaobject classes.

** Modularity of the code **

The actual caching algorithm is isolated in a modest number of functions.
The code which generates cache lookup code is all found in cache.lisp and
dlap.lisp.  Certain non-wrapper-caching special cases are in this file.


** Handling the metacircularity **

In CLOS, method lookup is the potential source of infinite metacircular
regress.  The metaobject protocol specification gives us wide flexibility
in how to address this problem.  PCL uses a technique which handles the
problem not only for the metacircular language described in Chapter 3, but
also for the PCL protocol which includes additional generic functions
which control more aspects of the CLOS implementation.

The source of the metacircular regress can be seen in a number of ways.
One is that the specified method lookup protocol must, as part of doing
the method lookup (or at least the cache miss case), itself call generic
functions.  It is easy to see that if the method lookup for a generic
function ends up calling that same generic function there can be trouble.

Fortunately, there is an easy solution at hand.  The solution is based on 
the restriction that portable code cannot change the class of a specified
metaobject.  This restriction implies that for specified generic functions,
the method lookup protocol they follow is fixed.  

More precisely, for such specified generic functions, most generic functions
that are called during their own method lookup will not run portable methods. 
This allows the implementation to usurp the actual generic function call in
this case.  In short, method lookup of a standard generic function, in the
case where the only applicable methods are themselves standard doesn't
have to do any method lookup to implement itself.

And so, we are saved.

|#


;************************************************************************
;   temporary for data gathering          temporary for data gathering
;************************************************************************

(defvar *dfun-states* (make-hash-table :test #'eq))

(defun notice-dfun-state (generic-function state &optional nkeys valuep)
  (setf (gethash generic-function *dfun-states*) 
	(cons state (when nkeys (list nkeys valuep)))))

;************************************************************************
;   temporary for data gathering          temporary for data gathering
;************************************************************************



(defvar *dfun-constructors* ())			;An alist in which each entry is
						;of the form (<generator> . (<subentry> ...))
						;Each subentry is of the form:
						;  (<args> <constructor> <system>)

(defvar *enable-dfun-constructor-caching* t)	;If this is NIL, then the whole mechanism
						;for caching dfun constructors is turned
						;off.  The only time that makes sense is
						;when debugging LAP code. 

(defun show-dfun-constructors ()
  (format t "~&DFUN constructor caching is ~A." (if *enable-dfun-constructor-caching*
						    "enabled"
						    "disabled"))
  (dolist (generator-entry *dfun-constructors*)
    (dolist (args-entry (cdr generator-entry))
      (format t "~&~S ~S"
	      (cons (car generator-entry) (caar args-entry))
	      (caddr args-entry)))))

(defun get-dfun-constructor (generator &rest args)
  (let* ((generator-entry (assq generator *dfun-constructors*))
	 (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (if (null *enable-dfun-constructor-caching*)
	(apply (symbol-function generator) args)
	(or (cadr args-entry)
	    (let ((new (apply (symbol-function generator) args)))
	      (if generator-entry
		  (push (list (copy-list args) new nil) (cdr generator-entry))
		  (push (list generator (list (copy-list args) new nil)) *dfun-constructors*))
	      new)))))

(defun load-precompiled-dfun-constructor (generator args system constructor)
  (let* ((generator-entry (assq generator *dfun-constructors*))
	 (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (unless args-entry
      (if generator-entry
	  (push (list args constructor system) (cdr generator-entry))
	  (push (list generator (list args constructor system)) *dfun-constructors*)))))

(defmacro precompile-dfun-constructors (&optional system)
  #+excl ()
  #-excl
  (let ((*precompiling-lap* t))
    `(progn
       ,@(gathering1 (collecting)
	   (dolist (generator-entry *dfun-constructors*)
	     (dolist (args-entry (cdr generator-entry))
	       (when (or (null (caddr args-entry))
			 (eq (caddr args-entry) system))
		 (multiple-value-bind (closure-variables arguments iregs vregs tregs lap)
		     (apply (symbol-function (car generator-entry)) (car args-entry))
		   (gather1
		     (make-top-level-form `(precompile-dfun-constructor ,(car generator-entry))
					  '(load)
		       `(load-precompiled-dfun-constructor
			  ',(car generator-entry)
			  ',(car args-entry)
			  ',system
			  (precompile-lap-closure-generator ,closure-variables
							    ,arguments
							    ,iregs
							    ,vregs
							    ,tregs
							    ,lap)))))))))
      (invalidate-uncompiled-discriminating-functions))))



(defun make-initial-dfun (generic-function)
  #'(lambda (&rest args)
      #+Genera (declare (dbg:invisible-frame :pcl-internals))
      (initial-dfun args generic-function)))
    

;;;
;;; When all the methods of a generic function are automatically generated
;;; reader or writer methods a number of special optimizations are possible.
;;; These are important because of the large number of generic functions of
;;; this type.
;;;
;;; There are a number of cases:
;;;
;;;   ONE-CLASS-ACCESSOR
;;;     In this case, the accessor generic function has only been called
;;;     with one class of argument.  There is no cache vector, the wrapper
;;;     of the one class, and the slot index are stored directly as closure
;;;     variables of the discriminating function.  This case can convert to
;;;     either of the next kind.
;;;
;;;   TWO-CLASS-ACCESSOR
;;;     Like above, but two classes.  This is common enough to do specially.
;;;     There is no cache vector.  The two classes are stored a separate
;;;     closure variables.
;;;
;;;   ONE-INDEX-ACCESSOR
;;;     In this case, the accessor generic function has seen more than one
;;;     class of argument, but the index of the slot is the same for all
;;;     the classes that have been seen.  A cache vector is used to store
;;;     the wrappers that have been seen, the slot index is stored directly
;;;     as a closure variable of the discriminating function.  This case
;;;     can convert to the next kind.
;;;
;;;   N-N-ACCESSOR
;;;     This is the most general case.  In this case, the accessor generic
;;;     function has seen more than one class of argument and more than one
;;;     slot index.  A cache vector stores the wrappers and corresponding
;;;     slot indexes.  Because each cache line is more than one element
;;;     long, a cache lock count is used.
;;;


;;;
;;; ONE-CLASS-ACCESSOR
;;;
(defun update-to-one-class-readers-dfun (generic-function wrapper index)
  (let ((constructor (get-dfun-constructor 'emit-one-class-reader (consp index))))
    (notice-dfun-state generic-function `(one-class readers ,(consp index)))	;***
    (update-dfun
      generic-function
      (funcall constructor
	       wrapper
	       index
	       #'(lambda (arg)
		   (declare (pcl-fast-call))
		   (one-class-readers-miss
		     arg generic-function index wrapper))))))

(defun update-to-one-class-writers-dfun (generic-function wrapper index)
  (let ((constructor (get-dfun-constructor 'emit-one-class-writer (consp index))))
    (notice-dfun-state generic-function `(one-class writers ,(consp index)))	;***
    (update-dfun
      generic-function
      (funcall constructor
	       wrapper
	       index
	       #'(lambda (new-value arg)
		   (declare (pcl-fast-call))
		   (one-class-writers-miss
		     new-value arg generic-function index wrapper))))))

(defun one-class-readers-miss (arg generic-function index wrapper)
  (accessor-miss
    generic-function 'one-class 'reader nil arg index wrapper nil nil nil))

(defun one-class-writers-miss (new arg generic-function index wrapper)
  (accessor-miss
    generic-function 'one-class 'writer new arg index wrapper nil nil nil))


;;;
;;; TWO-CLASS-ACCESSOR
;;;
(defun update-to-two-class-readers-dfun
       (generic-function wrapper-0 wrapper-1 index)
  (let ((constructor (get-dfun-constructor 'emit-two-class-reader (consp index))))
    (notice-dfun-state generic-function `(two-class readers ,(consp index)))	;***
    (update-dfun
      generic-function
      (funcall constructor
	       wrapper-0 wrapper-1 index
	       #'(lambda (arg)
		   (declare (pcl-fast-call))
		   (two-class-readers-miss 
		     arg
		     generic-function index wrapper-0 wrapper-1))))))

(defun update-to-two-class-writers-dfun
       (generic-function wrapper-0 wrapper-1 index)
  (let ((constructor (get-dfun-constructor 'emit-two-class-writer (consp index))))
    (notice-dfun-state generic-function `(two-class writers ,(consp index)))	;***
    (update-dfun
      generic-function
      (funcall constructor
	       wrapper-0 wrapper-1 index
	       #'(lambda (new-value arg)
		   (declare (pcl-fast-call))
		   (two-class-writers-miss
		     new-value arg
		     generic-function index wrapper-0 wrapper-1))))))

(defun two-class-readers-miss (arg generic-function index w0 w1)
  (accessor-miss
    generic-function 'two-class 'reader nil arg index w0 w1 nil nil))

(defun two-class-writers-miss (new arg generic-function index w0 w1)
  (accessor-miss
    generic-function 'two-class 'writer new arg index w0 w1 nil nil))



;;;
;;; std accessors same index dfun
;;;
(defun update-to-one-index-readers-dfun
       (generic-function index &optional field cache)
  (unless field (setq field (wrapper-field 'number)))
  (let ((constructor (get-dfun-constructor 'emit-one-index-readers (consp index))))
    (multiple-value-bind (mask size)
	(compute-cache-parameters 1 nil (or cache 4))
      (unless cache (setq cache (get-cache size)))
      (notice-dfun-state generic-function `(one-index readers ,(consp index)))	;***
      (update-dfun
	generic-function
	(funcall constructor
		 field cache mask size index
		 #'(lambda (arg)
		     (declare (pcl-fast-call))
		     (one-index-readers-miss
		       arg generic-function index field cache)))
	cache))))

(defun update-to-one-index-writers-dfun
       (generic-function index &optional field cache)
  (unless field (setq field (wrapper-field 'number)))
  (let ((constructor (get-dfun-constructor 'emit-one-index-writers (consp index))))
    (multiple-value-bind (mask size)
	(compute-cache-parameters 1 nil (or cache 4))
      (unless cache (setq cache (get-cache size)))
      (notice-dfun-state generic-function `(one-index writers ,(consp index)))	;***
      (update-dfun
	generic-function
	(funcall constructor
		 field cache mask size index 
		 #'(lambda (new-value arg)
		     (declare (pcl-fast-call))
		     (one-index-writers-miss
		       new-value arg generic-function index field cache)))
	cache))))

(defun one-index-readers-miss (arg gf index field cache)
  (accessor-miss
    gf 'one-index 'reader nil arg index nil nil field cache))

(defun one-index-writers-miss (new arg gf index field cache)
  (accessor-miss
    gf 'one-index 'writer new arg index nil nil field cache))


(defun one-index-limit-fn (nlines)
  (default-limit-fn nlines))



(defun update-to-n-n-readers-dfun (generic-function &optional field cache)
  (unless field (setq field (wrapper-field 'number)))
  (let ((constructor (get-dfun-constructor 'emit-n-n-readers)))
    (multiple-value-bind (mask size)
	(compute-cache-parameters 1 t (or cache 2))
      (unless cache (setq cache (get-cache size)))
      (notice-dfun-state generic-function `(n-n readers))	;***
      (update-dfun
	generic-function
	(funcall constructor
		 field cache mask size
		 #'(lambda (arg)
		     (declare (pcl-fast-call))
		     (n-n-readers-miss
		       arg generic-function field cache)))
	cache))))

(defun update-to-n-n-writers-dfun (generic-function &optional field cache)
  (unless field (setq field (wrapper-field 'number)))
  (let ((constructor (get-dfun-constructor 'emit-n-n-writers)))
    (multiple-value-bind (mask size)
	(compute-cache-parameters 1 t (or cache 2))
      (unless cache (setq cache (get-cache size)))
      (notice-dfun-state generic-function `(n-n writers))	;***
      (update-dfun
	generic-function
	(funcall constructor
		 field cache mask size
		 #'(lambda (new arg)
		     (declare (pcl-fast-call))
		     (n-n-writers-miss
		       new arg generic-function field cache)))
	cache))))

(defun n-n-readers-miss (arg gf field cache)
  (accessor-miss gf 'n-n 'reader nil arg nil nil nil field cache))

(defun n-n-writers-miss (new arg gf field cache)
  (accessor-miss gf 'n-n 'writer new arg nil nil nil field cache))


(defun n-n-accessors-limit-fn (nlines)
  (default-limit-fn nlines))


;;;
;;;
;;;
(defun update-to-checking-dfun (generic-function function &optional field
								    cache)
  (unless field (setq field (wrapper-field 'number)))
  (let* ((arg-info (gf-arg-info generic-function))
	 (metatypes (arg-info-metatypes arg-info))
	 (applyp (arg-info-applyp arg-info))
	 (nkeys (arg-info-nkeys arg-info)))
    (if (every #'(lambda (mt) (eq mt 't)) metatypes)
	(progn
	  (notice-dfun-state generic-function `(default-method-only))	;***
	  (update-dfun generic-function function))
	(multiple-value-bind (mask size)
	    (compute-cache-parameters nkeys nil (or cache 2))
	  (unless cache (setq cache (get-cache size)))
	  (let ((constructor (get-dfun-constructor 'emit-checking metatypes applyp)))
	    (notice-dfun-state generic-function '(checking) nkeys nil) ;****
	    (update-dfun
	      generic-function
	      (funcall constructor
		       field cache mask size function
		       #'(lambda (&rest args)
			   (declare (pcl-fast-call))
			   (checking-miss generic-function args function field cache)))
	      cache))))))


(defun checking-limit-fn (nlines)
  (default-limit-fn nlines))


;;;
;;;
;;;
(defun update-to-caching-dfun (generic-function &optional field cache)
  (unless field (setq field (wrapper-field 'number)))
  (let* ((arg-info (gf-arg-info generic-function))
	 (metatypes (arg-info-metatypes arg-info))
	 (applyp (arg-info-applyp arg-info))
	 (nkeys (arg-info-nkeys arg-info))
	 (constructor (get-dfun-constructor 'emit-caching metatypes applyp)))
    (multiple-value-bind (mask size)
	(compute-cache-parameters nkeys t (or cache 2))
      (unless cache (setq cache (get-cache size)))
      (notice-dfun-state generic-function '(caching) nkeys t) ;****
      (update-dfun
	generic-function
	(funcall constructor
		 field cache mask size
		 #'(lambda (&rest args) 
		     (declare (pcl-fast-call))
		     (caching-miss generic-function args field cache)))
	cache))))


(defun caching-limit-fn (nlines)
  (default-limit-fn nlines))



;;;
;;; The dynamically adaptive method lookup algorithm is implemented is
;;; implemented as a kind of state machine.  The kinds of discriminating
;;; function is the state, the various kinds of reasons for a cache miss
;;; are the state transitions.
;;;
;;; The code which implements the transitions is all in the miss handlers
;;; for each kind of dfun.  Those appear here.
;;;
;;; Note that within the states that cache, there are dfun updates which
;;; simply select a new cache or cache field.  Those are not considered
;;; as state transitions.
;;; 
(defun initial-dfun (args generic-function)
  #+Genera (declare (dbg:invisible-frame :pcl-internals))
  (protect-cache-miss-code generic-function
			   args
    (multiple-value-bind (wrappers invalidp nfunction applicable)
	(cache-miss-values generic-function args)
      (multiple-value-bind (ntype nindex)
	  (accessor-miss-values generic-function applicable args)
	(cond ((null applicable)
	       (apply #'no-applicable-method generic-function args))
	      (invalidp
	       (apply nfunction args))
	      ((and ntype nindex)
	       (ecase ntype
		 (reader (update-to-one-class-readers-dfun generic-function wrappers nindex))
		 (writer (update-to-one-class-writers-dfun generic-function wrappers nindex)))
	       (apply nfunction args))
	      (ntype
	       (apply nfunction args))
	      (t
	       (update-to-checking-dfun generic-function nfunction)
	       (apply nfunction args)))))))

(defun accessor-miss (gf ostate otype new object oindex ow0 ow1 field cache)
  (declare (ignore ow1))
  (let ((args (ecase otype			;The congruence rules assure
		(reader (list object))		;us that this is safe despite
		(writer (list new object)))))	;not knowing the new type yet.
    
    (protect-cache-miss-code gf
			     args
      (multiple-value-bind (wrappers invalidp nfunction applicable)
	  (cache-miss-values gf args)
	(multiple-value-bind (ntype nindex)
	    (accessor-miss-values gf applicable args)
	  ;;
	  ;; The following lexical functions change the state of the
	  ;; dfun to that which is their name.  They accept arguments
	  ;; which are the parameters of the new state, and get other
	  ;; information from the lexical variables bound above.
	  ;; 
	  (flet ((two-class (index w0 w1)
		   (when (zerop (random 2)) (psetf w0 w1 w1 w0))
		   (ecase ntype
		     (reader (update-to-two-class-readers-dfun gf w0 w1 index))
		     (writer (update-to-two-class-writers-dfun gf w0 w1 index))
		     ))
		 (one-index (index &optional field cache)
		   (ecase ntype
		     (reader
		       (update-to-one-index-readers-dfun gf index field cache))
		     (writer
		       (update-to-one-index-writers-dfun gf index field cache))
		     ))
		 (n-n (&optional field cache)
		   (ecase ntype
		     (reader (update-to-n-n-readers-dfun gf field cache))
		     (writer (update-to-n-n-writers-dfun gf field cache))))
		 (checking ()
		   (update-to-checking-dfun gf nfunction))
		 ;;
		 ;;
		 ;;		 
		 (do-fill (valuep limit-fn update-fn)
		   (multiple-value-bind (nfield ncache)
		       (fill-cache field cache
				   1 valuep
				   limit-fn wrappers nindex)
		     (unless (and (= nfield field)
				  (eq ncache cache))
		       (funcall update-fn nfield ncache)))))

	    (cond ((null nfunction)
                   (apply #'no-applicable-method gf args))
		  ((null ntype)
		   (checking)
		   (apply nfunction args))
                  ((or invalidp
                       (null nindex))
                   (apply nfunction args))
		  ((not (or (std-instance-p object)
			    (fsc-instance-p object)))
		   (checking)
		   (apply nfunction args))
		  ((neq ntype otype)
		   (checking)
		   (apply nfunction args))
		  (t
		   (ecase ostate
		     (one-class
		       (if (eql nindex oindex)
			   (two-class nindex ow0 wrappers)
			   (n-n)))
		     (two-class
		       (if (eql nindex oindex)
			   (one-index nindex)
			   (n-n)))
		     (one-index
		       (if (eql nindex oindex)
			   (do-fill nil
				    #'one-index-limit-fn
				    #'(lambda (nfield ncache)
					(one-index nindex nfield ncache)))
			   (n-n)))
		     (n-n
		       (unless (consp nindex)
			 (do-fill t
				  #'n-n-accessors-limit-fn
				  #'n-n))))
		   (apply nfunction args)))))))))



(defun checking-miss (generic-function args ofunction field cache)
  (protect-cache-miss-code generic-function
			   args
    (let* ((arg-info (gf-arg-info generic-function))
	   (nkeys (arg-info-nkeys arg-info)))
      (multiple-value-bind (wrappers invalidp nfunction)
	  (cache-miss-values generic-function args)
	(cond (invalidp
	       (apply nfunction args))
	      ((null nfunction)
	       (apply #'no-applicable-method generic-function args))
	      ((eq ofunction nfunction)
	       (multiple-value-bind (nfield ncache)
		   (fill-cache field cache nkeys nil #'checking-limit-fn wrappers nil)
		 (unless (and (= nfield field)
			      (eq ncache cache))
		   (update-to-checking-dfun generic-function
					    nfunction nfield ncache)))
	       (apply nfunction args))
	      (t
	       (update-to-caching-dfun generic-function)
	       (apply nfunction args)))))))

(defun caching-miss (generic-function args ofield ocache)
  (protect-cache-miss-code generic-function
			   args
    (let* ((arg-info (gf-arg-info generic-function))
	   (nkeys (arg-info-nkeys arg-info)))
      (multiple-value-bind (wrappers invalidp function)
	  (cache-miss-values generic-function args)
	(cond (invalidp
	       (apply function args))
	      ((null function)
	       (apply #'no-applicable-method generic-function args))
	      (t
	       (multiple-value-bind (nfield ncache)
		   (fill-cache ofield ocache nkeys t #'caching-limit-fn wrappers function)
		 (unless (and (= nfield ofield)
			      (eq ncache ocache))
		   (update-to-caching-dfun generic-function nfield ncache)))
	       (apply function args)))))))


;;;
;;; Some useful support functions which are shared by the implementations of
;;; the different kinds of dfuns.
;;;



;;;
;;; Given a generic function and a set of arguments to that generic function,
;;; returns a mess of values.
;;;
;;;  <wrappers>   Is a single wrapper if the generic function has only
;;;               one key, that is arg-info-nkeys of the arg-info is 1.
;;;               Otherwise a list of the wrappers of the specialized
;;;               arguments to the generic function.
;;;
;;;               Note that all these wrappers are valid.  This function
;;;               does invalid wrapper traps when it finds an invalid
;;;               wrapper and then returns the new, valid wrapper.
;;;
;;;  <invalidp>   True if any of the specialized arguments had an invalid
;;;               wrapper, false otherwise.
;;;
;;;  <function>   The compiled effective method function for this set of
;;;               arguments.  Gotten from get-secondary-dispatch-function
;;;               so effective-method-function caching is in effect, and
;;;               that is important since it is what keeps us in checking
;;;               dfun state when possible.
;;;
;;;  <type>       READER or WRITER when the only method that would be run
;;;               is a standard reader or writer method.  To be specific,
;;;               the value is READER when the method combination is eq to
;;;               *standard-method-combination*; there are no applicable
;;;               :before, :after or :around methods; and the most specific
;;;               primary method is a standard reader method.
;;;
;;;  <index>      If <type> is READER or WRITER, and the slot accessed is
;;;               an :instance slot, this is the index number of that slot
;;;               in the object argument.
;;;
;;;  <applicable> Sorted list of applicable methods.
;;;
;;;
(defun cache-miss-values (generic-function args)
  (declare (values wrappers invalidp function applicable))
  (multiple-value-bind (function appl arg-info)
      (get-secondary-dispatch-function generic-function args)
    (multiple-value-bind (wrappers invalidp)
	(get-wrappers generic-function args arg-info)
      (values wrappers invalidp 
	      (cache-miss-values-function generic-function function)
	      appl))))

(defun get-wrappers (generic-function args &optional arg-info)
  (let* ((invalidp nil)
	 (wrappers ())
	 (arg-info (or arg-info (gf-arg-info generic-function)))
	 (metatypes (arg-info-metatypes arg-info))
	 (nkeys (arg-info-nkeys arg-info)))
    (flet ((get-valid-wrapper (x)
	     (let ((wrapper (wrapper-of x)))
	       (cond ((invalid-wrapper-p wrapper)
		      (setq invalidp t)
		      (check-wrapper-validity x))
		     (t wrapper)))))
      (setq wrappers
	    (block collect-wrappers
	      (gathering1 (collecting)
		(iterate ((arg (list-elements args))
			  (metatype (list-elements metatypes)))
		  (when (neq metatype 't)
		    (if (= nkeys 1)
			(return-from collect-wrappers
			  (get-valid-wrapper arg))
			(gather1 (get-valid-wrapper arg))))))))
      (values wrappers invalidp))))

(defun cache-miss-values-function (generic-function function)
  #+(and excl sun4) 
  (when (and (consp function) 
	     (eq 'lambda (car function)))
    (setq function (comp::.primcall 'make-interp-function-obj function)))
  (if (eq *generate-random-code-segments* generic-function)
      (progn
	(setq *generate-random-code-segments* nil) 
	#'(lambda (&rest args) (declare (ignore args)) nil))
      function))

(defun generate-random-code-segments (generic-function)
  (dolist (arglist (generate-arglists generic-function))
    (let ((*generate-random-code-segments* generic-function))
      (apply generic-function arglist))))

(defun generate-arglists (generic-function)
  ;;Generate arglists using class-prototypes and specializer-objects
  ;;to get all the "different" values that could be returned by 
  ;;get-secondary-dispatch-function for this generic-function.
  nil)

(defun accessor-miss-values (generic-function applicable args)
  (declare (values type index))
  (let ((type (and (eq (generic-function-method-combination generic-function)
		       *standard-method-combination*)
		   (every #'(lambda (m) (null (method-qualifiers m))) applicable)
		   (let ((method (car applicable)))
		     (cond ((standard-reader-method-p method)
			    (and (optimize-slot-value-by-class-p
				  (class-of (car args))
				  (accessor-method-slot-name method)
				  'slot-value)
				 'reader))
			   ((standard-writer-method-p method)
			    (and (optimize-slot-value-by-class-p
				  (class-of (cadr args))
				  (accessor-method-slot-name method)
				  'set-slot-value)
				 'writer))
			   (t nil))))))
    (values type
	    (and type
		 (let ((wrapper (wrapper-of (case type
					      (reader (car args))
					      (writer (cadr args)))))
		       (slot-name (accessor-method-slot-name (car applicable))))
		   (or (instance-slot-index wrapper slot-name)
		       (assq slot-name (wrapper-class-slots wrapper))))))))

