;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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

(eval-when (compile load eval)
  
(defvar *defclass-times*   '(load eval))	;Probably have to change this
						;if you use defconstructor.
(defvar *defmethod-times*  '(load eval))
(defvar *defgeneric-times* '(load eval))

)

;;;
;;; MAKE-TOP-LEVEL-FORM is used by all PCL macros that appear `at top-level'.
;;;
;;; The original motiviation for this function was to deal with the bug in
;;; the Genera compiler that prevents lambda expressions in top-level forms
;;; other than DEFUN from being compiled.
;;;
;;; Now this function is used to grab other functionality as well.  This
;;; includes:
;;;   - Preventing the grouping of top-level forms.  For example, a
;;;     DEFCLASS followed by a DEFMETHOD may not want to be grouped
;;;     into the same top-level form.
;;;   - Telling the programming environment what the pretty version
;;;     of the name of this form is.  This is used by WARN.
;;; 
(defun make-top-level-form (name times form)
  (flet ((definition-name ()
	   (if (and (listp name)
		    (memq (car name) '(defmethod defclass class method method-combination)))
	       (format nil "~A~{ ~S~}"
		       (capitalize-words (car name) ()) (cdr name))
	       (format nil "~S" name))))
    (definition-name)
    #+Genera
    (progn
      #-Genera-Release-8
      (let ((thunk-name (gensym "TOP-LEVEL-FORM")))
	`(eval-when ,times
	   (defun ,thunk-name ()
	     (declare (sys:function-parent
			,(cond ((listp name)
				(case (first name)
				  (defmethod `(method ,@(rest name)))
				  (otherwise (second name))))
			       (t name))
			,(cond ((listp name)
				(case (first name)
				  ((defmethod defgeneric) 'defun)
				  ((defclass) 'defclass)
				  (otherwise (first name))))
			       (t 'defun))))
	     ,form)
	   (,thunk-name)))
      #+Genera-Release-8
      `(compiler-let ((compiler:default-warning-function ',name))
	 (eval-when ,times
	   (funcall #'(lambda ()
			(declare ,(cond ((listp name)
					 (case (first name)
					   ((defclass)
					    `(sys:function-parent ,(second name) defclass))
					   ((defmethod)
					    `(sys:function-name (method ,@(rest name))))
					   ((defgeneric)
					    `(sys:function-name ,(second name)))
					   (otherwise
					     `(sys:function-name ,name))))
					(t
					 `(sys:function-name ,name))))
			,form)))))
    #+LCL3.0
    `(compiler-let ((lucid::*compiler-message-string*
		      (or lucid::*compiler-message-string*
			  ,(definition-name))))
       (eval-when ,times ,form))
    #-(or Genera LCL3.0)
    (make-progn `',name `(eval-when ,times ,form))))

(defun make-progn (&rest forms)
  (let ((progn-form nil))
    (labels ((collect-forms (forms)
	       (unless (null forms)
		 (collect-forms (cdr forms))
		 (if (and (listp (car forms))
			  (eq (caar forms) 'progn))
		     (collect-forms (cdar forms))
		     (push (car forms) progn-form)))))
      (collect-forms forms)
      (cons 'progn progn-form))))



;;; 
;;; Like the DEFMETHOD macro, the expansion of the DEFCLASS macro is fixed.
;;; DEFCLASS always expands into a call to LOAD-DEFCLASS.  Until the meta-
;;; braid is set up, LOAD-DEFCLASS has a special definition which simply
;;; collects all class definitions up, when the metabraid is initialized it
;;; is done from those class definitions.
;;;
;;; After the metabraid has been setup, and the protocol for defining classes
;;; has been defined, the real definition of LOAD-DEFCLASS is installed by the
;;; file defclass.lisp
;;; 
(defmacro DEFCLASS (name direct-superclasses direct-slots &rest options)
  (declare (indentation 2 4 3 1))
  (expand-defclass name direct-superclasses direct-slots options))

(defun expand-defclass (name supers slots options)
  (setq supers  (copy-tree supers)
	slots   (copy-tree slots)
	options (copy-tree options))
  (let ((metaclass 'standard-class))
    (dolist (option options)
      (if (not (listp option))
          (error "~S is not a legal defclass option." option)
          (when (eq (car option) ':metaclass)
            (unless (legal-class-name-p (cadr option))
              (error "The value of the :metaclass option (~S) is not a~%~
                      legal class name."
                     (cadr option)))
            (setq metaclass (cadr option))
	    (setf options (remove option options))
	    (return t))))

    (let ((*initfunctions* ())
	  (*accessors* ()))			;Truly a crock, but we got
						;to have it to live nicely.
      (declare (special *initfunctions* *accessors*))
      (let ((canonical-slots
	      (mapcar #'(lambda (spec)
			  (canonicalize-slot-specification name spec))
		      slots))
	    (other-initargs
	      (mapcar #'(lambda (option)
			  (canonicalize-defclass-option name option))
		      options)))
	(do-standard-defsetfs-for-defclass *accessors*)
	(make-top-level-form `(defclass ,name)
			     *defclass-times*
	  `(let ,(mapcar #'cdr *initfunctions*)
	     (load-defclass ',name
			    ',metaclass
			    ',supers
			    (list ,@canonical-slots)
			    (list ,@(apply #'append other-initargs))
			    ',*accessors*)))))))

(defun make-initfunction (initform)
  (declare (special *initfunctions*))
  (cond ((or (eq initform 't)
	     (equal initform ''t))
	 '(function true))
	((or (eq initform 'nil)
	     (equal initform ''nil))
	 '(function false))
	((or (eql initform '0)
	     (equal initform ''0))
	 '(function zero))
	(t
	 (let ((entry (assoc initform *initfunctions* :test #'equal)))
	   (unless entry
	     (setq entry (list initform
			       (gensym)
			       `(function (lambda () ,initform))))
	     (push entry *initfunctions*))
	   (cadr entry)))))

(defun canonicalize-slot-specification (class-name spec)
  (declare (special *accessors*))
  (cond ((and (symbolp spec)
	      (not (keywordp spec))
	      (not (memq spec '(t nil))))		   
	 `'(:name ,spec))
	((not (consp spec))
	 (error "~S is not a legal slot specification." spec))
	((null (cdr spec))
	 `'(:name ,(car spec)))
	((null (cddr spec))
	 (error "In DEFCLASS ~S, the slot specification ~S is obsolete.~%~
                 Convert it to ~S"
		class-name spec (list (car spec) :initform (cadr spec))))
	(t
	 (let* ((name (pop spec))
		(readers ())
		(writers ())
		(initargs ())
		(unsupplied (list nil))
		(initform (getf spec :initform unsupplied)))
	   (doplist (key val) spec
	     (case key
	       (:accessor (push val *accessors*)
			  (push val readers)
			  (push `(setf ,val) writers))
	       (:reader   (push val readers))
	       (:writer   (push val writers))
	       (:initarg  (push val initargs))))
	   (loop (unless (remf spec :accessor) (return)))
	   (loop (unless (remf spec :reader)   (return)))
	   (loop (unless (remf spec :writer)   (return)))
	   (loop (unless (remf spec :initarg)  (return)))
	   (setq spec `(:name     ',name
			:readers  ',readers
			:writers  ',writers
			:initargs ',initargs
			',spec))
	   (if (eq initform unsupplied)
	       `(list* ,@spec)
	       `(list* :initfunction ,(make-initfunction initform) ,@spec))))))
						
(defun canonicalize-defclass-option (class-name option)  
  (declare (ignore class-name))
  (case (car option)
    (:default-initargs
      (let ((canonical ()))
	(let (key val (tail (cdr option)))
	  (loop (when (null tail) (return nil))
		(setq key (pop tail)
		      val (pop tail))
		(push ``(,',key ,,(make-initfunction val) ,',val) canonical))
	  `(':direct-default-initargs (list ,@(nreverse canonical))))))
    (otherwise
      `(',(car option) ',(cdr option)))))


;;;
;;; This is the early definition of load-defclass.  It just collects up all
;;; the class definitions in a list.  Later, in the file braid1.lisp, these
;;; are actually defined.
;;;


;;;
;;; Each entry in *early-class-definitions* is an early-class-definition.
;;; 
;;;
(defparameter *early-class-definitions* ())

(defun make-early-class-definition
       (name source metaclass
	superclass-names canonical-slots other-initargs)
  (list 'early-class-definition
	name source metaclass
	superclass-names canonical-slots other-initargs))
  
(defun ecd-class-name        (ecd) (nth 1 ecd))
(defun ecd-source            (ecd) (nth 2 ecd))
(defun ecd-metaclass         (ecd) (nth 3 ecd))
(defun ecd-superclass-names  (ecd) (nth 4 ecd))
(defun ecd-canonical-slots   (ecd) (nth 5 ecd))
(defun ecd-other-initargs    (ecd) (nth 6 ecd))

(proclaim '(notinline load-defclass))
(defun load-defclass
       (name metaclass supers canonical-slots canonical-options accessor-names)
  (setq supers  (copy-tree supers)
	canonical-slots   (copy-tree canonical-slots)
	canonical-options (copy-tree canonical-options))
  (do-standard-defsetfs-for-defclass accessor-names)
  (let ((ecd
	  (make-early-class-definition name
				       (load-truename)
				       metaclass
				       supers
				       canonical-slots
				       (apply #'append canonical-options)))
	(existing
	  (find name *early-class-definitions* :key #'ecd-class-name)))
    (setq *early-class-definitions*
	  (cons ecd (remove existing *early-class-definitions*)))
    ecd))

