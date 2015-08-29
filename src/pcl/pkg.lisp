;;;-*-Mode:LISP; Package:(PCL (LISP WALKER)); Base:10; Syntax:Common-lisp -*-
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
;;; Some CommonLisps have more symbols in the Lisp package than the ones that
;;; are explicitly specified in CLtL.  This causes trouble. Any Lisp that has
;;; extra symbols in the Lisp package should shadow those symbols in the PCL
;;; package.
;;;
#+TI
(shadow '(string-append once-only destructuring-bind
	  memq assq delq neq true false
	  without-interrupts
	  defmethod)
	*the-pcl-package*)

#+GCLisp
(shadow '(string-append memq assq delq neq make-instance)
	*the-pcl-package*)

#+Genera
(shadowing-import '(zl:arglist zwei:indentation) *the-pcl-package*)

#+Lucid 
(import #-LCL3.0 'system:arglist
	#+LCL3.0 'lcl:arglist
	*the-pcl-package*)

#+WCL
(shadow 'destructuring-bind)

(defun wcl-make-symbol (name)
  ;; PCL use of make-symbol doesn't work well with static compilation.
  ;; Need to fix wcl, but for now...
  (gentemp name))



(shadow 'documentation)


;;;						
;;; These come from the index pages of 88-002R.
;;;
;;;
(eval-when (compile load eval)  
  
(defvar *exports* '(add-method
		    built-in-class
		    call-method
		    call-next-method
		    change-class
		    class-name
		    class-of
		    compute-applicable-methods
		    defclass
		    defgeneric
		    define-method-combination
		    defmethod
		    ensure-generic-function
		    find-class
		    find-method
		    function-keywords
		    generic-flet
		    generic-labels
		    initialize-instance
		    invalid-method-error
		    make-instance
		    make-instances-obsolete
		    method-combination-error
		    method-qualifiers
		    next-method-p
		    no-applicable-method
		    no-next-method
		    print-object
		    reinitialize-instance
		    remove-method
		    shared-initialize
		    slot-boundp
		    slot-exists-p
		    slot-makunbound
		    slot-missing
		    slot-unbound
		    slot-value
		    standard
		    standard-class
		    standard-generic-function
		    standard-method
		    standard-object
		    structure-class
		    symbol-macrolet
		    update-instance-for-different-class
		    update-instance-for-redefined-class
		    with-accessors
		    with-added-methods
		    with-slots
		    ))

);eval-when 

#-(or KCL IBCL)
(export *exports* *the-pcl-package*)

#+(or KCL IBCL)
(mapc 'export (list *exports*) (list *the-pcl-package*))



;(defvar *chapter-3-exports* '(
;			  get-setf-function
;			  get-setf-function-name
;
;			  class-prototype
;			  class
;			  object
;
;;			  essential-class
;			   
;			  class-name
;			  class-precedence-list
;			  class-local-supers
;			  class-local-slots
;			  class-direct-subclasses
;			  class-direct-methods
;			  class-slots
;
;			   
;			  method-arglist
;			  method-argument-specifiers			
;			  method-function
;			   
;			  method-equal
;			   
;			  slotd-name
;			  slot-missing
;			   
;;			  define-meta-class
;;			  %allocate-instance
;;			  %instance-ref
;;			  %instancep
;;			  %instance-meta-class
;
;			  allocate-instance
;			  optimize-slot-value
;			  optimize-setf-of-slot-value
;			  add-named-class
;			  class-for-redefinition
;			  add-class
;			  supers-changed
;			  slots-changed
;			  check-super-metaclass-compatibility
;			  make-slotd
;			  compute-class-precedence-list
;			  walk-method-body
;			  walk-method-body-form
;			  add-named-method
;			  remove-named-method
;
;
;			  ))

