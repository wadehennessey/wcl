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
;;; This file contains portable versions of low-level functions and macros
;;; which are ripe for implementation specific customization.  None of the
;;; code in this file *has* to be customized for a particular Common Lisp
;;; implementation. Moreover, in some implementations it may not make any
;;; sense to customize some of this code.
;;;
;;; But, experience suggests that MOST Common Lisp implementors will want
;;; to customize some of the code in this file to make PCL run better in
;;; their implementation.  The code in this file has been separated and
;;; heavily commented to make that easier.
;;;
;;; Implementation-specific version of this file already exist for:
;;; 
;;;    Symbolics Genera family     genera-low.lisp
;;;    Lucid Lisp                  lucid-low.lisp
;;;    Xerox 1100 family           xerox-low.lisp
;;;    ExCL (Franz)                excl-low.lisp
;;;    Kyoto Common Lisp           kcl-low.lisp
;;;    Vaxlisp                     vaxl-low.lisp
;;;    CMU Lisp                    cmu-low.lisp
;;;    H.P. Common Lisp            hp-low.lisp
;;;    Golden Common Lisp          gold-low.lisp
;;;    Ti Explorer                 ti-low.lisp
;;;    
;;;
;;; These implementation-specific files are loaded after this file.  Because
;;; none of the macros defined by this file are used in functions defined by
;;; this file the implementation-specific files can just contain the parts of
;;; this file they want to change.  They don't have to copy this whole file
;;; and then change the parts they want.
;;;
;;; If you make changes or improvements to these files, or if you need some
;;; low-level part of PCL re-modularized to make it more portable to your
;;; system please send mail to CommonLoops.pa@Xerox.com.
;;;
;;; Thanks.
;;; 

(in-package 'pcl)

(defmacro %svref (vector index)
  `(locally (declare (optimize (speed 3) (safety 0))
		     (inline svref))
	    (svref (the simple-vector ,vector) (the fixnum ,index))))

(defsetf %svref (vector index) (new-value)
  `(locally (declare (optimize (speed 3) (safety 0))
		     (inline svref))
     (setf (svref (the simple-vector ,vector) (the fixnum ,index))
	   ,new-value)))


;;;
;;; without-interrupts
;;; 
;;; OK, Common Lisp doesn't have this and for good reason.  But For all of
;;; the Common Lisp's that PCL runs on today, there is a meaningful way to
;;; implement this.  WHAT I MEAN IS:
;;;
;;; I want the body to be evaluated in such a way that no other code that is
;;; running PCL can be run during that evaluation.  I agree that the body
;;; won't take *long* to evaluate.  That is to say that I will only use
;;; without interrupts around relatively small computations.
;;;
;;; INTERRUPTS-ON should turn interrupts back on if they were on.
;;; INTERRUPTS-OFF should turn interrupts back off.
;;; These are only valid inside the body of WITHOUT-INTERRUPTS.
;;;
;;; OK?
;;;
(defmacro without-interrupts (&body body)
  `(macrolet ((interrupts-on () ())
	      (interrupts-off () ()))
     (progn ,.body)))


;;;
;;;  Very Low-Level representation of instances with meta-class standard-class.
;;;
(defstruct (std-instance (:predicate std-instance-p)
			 (:conc-name %std-instance-)
			 (:constructor %%allocate-instance--class ())
			 (:print-function print-std-instance))
  (wrapper nil)
  (slots nil))

(defmacro std-instance-wrapper (x) `(%std-instance-wrapper ,x))
(defmacro std-instance-slots   (x) `(%std-instance-slots ,x))

(defun print-std-instance (instance stream depth) ;A temporary definition used
  (declare (ignore depth))		          ;for debugging the bootstrap
  (printing-random-thing (instance stream)        ;code of PCL (See high.lisp).
    (format stream "#<std-instance>")))

(defmacro %allocate-instance--class (no-of-slots)
  `(let ((instance (%%allocate-instance--class)))
     (%allocate-instance--class-1 ,no-of-slots instance)
     instance))

(defmacro %allocate-instance--class-1 (no-of-slots instance)
  (once-only (instance)
    `(progn 
       (setf (std-instance-slots ,instance)
	     (%allocate-static-slot-storage--class ,no-of-slots)))))

;;;
;;; This is the value that we stick into a slot to tell us that it is unbound.
;;; It may seem gross, but for performance reasons, we make this an interned
;;; symbol.  That means that the fast check to see if a slot is unbound is to
;;; say (EQ <val> '..SLOT-UNBOUND..).  That is considerably faster than looking
;;; at the value of a special variable.  Be careful, there are places in the
;;; code which actually use ..slot-unbound.. rather than this variable.  So
;;; much for modularity
;;; 
(defvar *slot-unbound* '..slot-unbound..)

(defmacro %allocate-static-slot-storage--class (no-of-slots)
  `(make-array ,no-of-slots :initial-element *slot-unbound*))


(defmacro std-instance-class (instance)
  `(wrapper-class (std-instance-wrapper ,instance)))



  ;;   
;;;;;; FUNCTION-ARGLIST
  ;;
;;; Given something which is functionp, function-arglist should return the
;;; argument list for it.  PCL does not count on having this available, but
;;; MAKE-SPECIALIZABLE works much better if it is available.  Versions of
;;; function-arglist for each specific port of pcl should be put in the
;;; appropriate xxx-low file. This is what it should look like:
;(defun function-arglist (function)
;  (<system-dependent-arglist-function> function))

(defun function-pretty-arglist (function)
  (declare (ignore function))
  ())

(defsetf function-pretty-arglist set-function-pretty-arglist)

(defun set-function-pretty-arglist (function new-value)
  (declare (ignore function))
  new-value)

;;;
;;; set-function-name
;;; When given a function should give this function the name <new-name>.
;;; Note that <new-name> is sometimes a list.  Some lisps get the upset
;;; in the tummy when they start thinking about functions which have
;;; lists as names.  To deal with that there is set-function-name-intern
;;; which takes a list spec for a function name and turns it into a symbol
;;; if need be.
;;;
;;; When given a funcallable instance, set-function-name MUST side-effect
;;; that FIN to give it the name.  When given any other kind of function
;;; set-function-name is allowed to return new function which is the 'same'
;;; except that it has the name.
;;;
;;; In all cases, set-function-name must return the new (or same) function.
;;; 
(defun set-function-name (function new-name)
  (declare (notinline set-function-name-1 intern-function-name))
  (set-function-name-1 function
		       (intern-function-name new-name)
		       new-name))

(defun set-function-name-1 (function new-name uninterned-name)
  (declare (ignore new-name uninterned-name))
  function)

(defun intern-function-name (name)
  (cond ((symbolp name) name)
	((listp name)
	 (intern (let ((*package* *the-pcl-package*)
		       (*print-case* :upcase)
		       (*print-gensym* 't))
		   (format nil "~S" name))
		 *the-pcl-package*))))


;;;
;;; COMPILE-LAMBDA
;;;
;;; This is like the Common Lisp function COMPILE.  In fact, that is what
;;; it ends up calling.  The difference is that it deals with things like
;;; watching out for recursive calls to the compiler or not calling the
;;; compiler in certain cases or allowing the compiler not to be present.
;;;
;;; This starts out with several variables and support functions which 
;;; should be conditionalized for any new port of PCL.  Note that these
;;; default to reasonable values, many new ports won't need to look at
;;; these values at all.
;;;
;;; *COMPILER-PRESENT-P*        NIL means the compiler is not loaded
;;;
;;; *COMPILER-SPEED*            one of :FAST :MEDIUM or :SLOW
;;;
;;; *COMPILER-REENTRANT-P*      T   ==> OK to call compiler recursively
;;;                             NIL ==> not OK
;;;
;;; function IN-THE-COMPILER-P  returns T if in the compiler, NIL otherwise
;;;                             This is not called if *compiler-reentrant-p*
;;;                             is T, so it only needs to be implemented for
;;;                             ports which have non-reentrant compilers.
;;;
;;;
(defvar *compiler-present-p* t)

(defvar *compiler-speed*
	#+(or KCL IBCL GCLisp) :slow
	#-(or KCL IBCL GCLisp) :fast)

(defvar *compiler-reentrant-p*
	#+(and (not XKCL) (or KCL IBCL)) nil
	#-(and (not XKCL) (or KCL IBCL)) t)

(defun in-the-compiler-p ()
  #+(and (not xkcl) (or KCL IBCL))compiler::*compiler-in-use*
  #+gclisp (typep (eval '(function (lambda ()))) 'lexical-closure)
  )

(defun compile-lambda (lambda &optional (desirability :fast))
  (cond ((null *compiler-present-p*)
	 (compile-lambda-uncompiled lambda))
	((and (null *compiler-reentrant-p*)
	      (in-the-compiler-p))
	 (compile-lambda-deferred lambda))
	((eq desirability :fast)
	 (compile nil lambda))
	((and (eq desirability :medium)
	      (member *compiler-speed* '(:fast :medium)))
	 (compile nil lambda))
	((and (eq desirability :slow)
	      (eq *compiler-speed* ':fast))
	 (compile nil lambda))
	(t
	  (compile-lambda-uncompiled lambda))))

(defun compile-lambda-uncompiled (uncompiled)
  #'(lambda (&rest args) (apply uncompiled args)))

(defun compile-lambda-deferred (uncompiled)
  (let ((compiled nil))
    #'(lambda (&rest args)
	(if compiled
	    (apply compiled args)
	    (if (in-the-compiler-p)
		(apply uncompiled args)
		(progn (setq compiled (compile nil uncompiled))
		       (apply compiled args)))))))

(defmacro precompile-random-code-segments (&optional system)
  `(progn
     (precompile-function-generators ,system)
     (precompile-dfun-constructors ,system)))



(defun record-definition (type spec &rest args)
  (declare (ignore type spec args))
  ())

(defun doctor-dfun-for-the-debugger (gf dfun) (declare (ignore gf)) dfun)

