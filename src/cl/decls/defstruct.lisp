;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defstruct struct-info
  name
  children
  slots
  length
  conc-name
  predicate
  include
  constructor
  copier
  print-function
  type
  named?
  initial-offset
  first-slot-index
  (constructor-arg-list :default)
  print-function-lambda-expr
  inherited-slot-overrides
  report-function
  report-function-lambda-expr
  dynamic?)
  
(defstruct struct-slot
  name
  initial-value
  type
  read-only?)

(defmacro defstruct (options &rest doc-string+slots)
  (let ((s (make-struct-info))
	(slots (if (stringp (car doc-string+slots))
		   (cdr doc-string+slots)
		   doc-string+slots)))
    (fill-in-defstruct-option-info s options)
    (setf (struct-info-slots s)
	  (mapcar #'parse-struct-slot-info slots))
    (let* ((all-slots (all-slots s))
	   (all-slot-names (mapcar #'struct-slot-name all-slots))
	   (first-slot-index
	    (+ (struct-info-initial-offset s)
	       (if (struct-info-named? s) 1 0))))
      (setf (struct-info-first-slot-index s) first-slot-index)
      (setf (struct-info-length s)
	    (+ (struct-info-first-slot-index s) (length all-slots)))
      `(progn
	(define-structure ,s)
	,@(if (null (struct-info-constructor s))
	      nil
	      (let* ((constructor-arg-list (constructor-arg-list s all-slots))
		     (non-required-constructor-args
		      (loop for r on constructor-arg-list
			    when (member (car r) lambda-list-keywords)
			    do (return r)))
		     (constructor-arg-slot-names
		      (loop for slot in all-slots
			    when (tree-find (struct-slot-name slot)
					    constructor-arg-list)
			    collect (struct-slot-name slot)))
		     (fixed-arg-constructor-name
		      (fixed-arg-constructor-name s
						  constructor-arg-slot-names)))
		`((defun-inline ,(struct-info-constructor s)
		      ,constructor-arg-list
		    (,fixed-arg-constructor-name ,@constructor-arg-slot-names))
		  (defun ,fixed-arg-constructor-name
		      ,constructor-arg-slot-names
		    (let ((s (,(struct-new-structure-function s)
			       ,(struct-info-length s))))
		      ,@(if (struct-info-named? s)
			    `((,(struct-set-slot-function s)
			       s
			       ,(1- first-slot-index)
			       ',(struct-info-name s)))
			    nil)
		      ,@(loop
			 for slot being the elements of all-slots
			 using (index i)
			 collect 
			 `(,(struct-set-slot-function s)
			   s ,
			   (+ first-slot-index i)
			   ,(let ((slot-name (struct-slot-name slot))) 
			      (if (and (member slot-name
					       constructor-arg-slot-names)
				       ;; Ridiculous Common Lisp Complexity!
				       ;; Use default value in slot spec
				       ;; if no EXPLICIT default is given
				       ;; in the constructor lambda-list. Ug.
				       (not (member
					     slot-name
					     non-required-constructor-args)))
				  slot-name
				  (struct-slot-initial-value slot)))))
		      s)))))
	,@(if (struct-info-named? s)
	      `((eval-when (eval)
		  (defun ,(predicate-name s) (s)
		    (and (typep s ',(struct-info-type s))
			 (let ((actual (,(struct-get-slot-function s)
					 s
					 ,(1- first-slot-index)))
			       (expected ',(struct-info-name s)))
			   (or (eq expected actual)
			       (inherited-structure-name-p
				expected actual)))))))
	      nil)
	,@(if (struct-info-named? s)
	      `((defun ,(fluid-predicate-name s) (s)
		  (and (typep s ',(struct-info-type s))
		       (let ((actual (,(struct-get-slot-function s)
				       s
				       ,(1- first-slot-index)))
			     (expected ',(struct-info-name s)))
			 (or (eq expected actual)
			     (inherited-structure-name-p expected actual))))))
	      nil)
	,@(loop for slot being the elements of all-slots using (index i)
	   collect
	   `(defmethod-inline ,(accessor-name s slot)
	     ((s ,(struct-type-check s)))
	     (,(struct-get-slot-function s) s ,(+ i first-slot-index))))
	,@(loop for slot being the elements of all-slots using (index i)
	   collect
	   `(defmethod-inline ,(updater-name s slot)
	     ((s ,(struct-type-check s)) value)
	     (,(struct-set-slot-function s) s ,(+ i first-slot-index) value)))
	,@(if (null (struct-info-print-function-lambda-expr s))
	      nil
	      `((defun ,(struct-info-print-function s)
		    ,@(cdr (struct-info-print-function-lambda-expr s)))))
	,@(if (null (struct-info-report-function-lambda-expr s))
	      nil
	      `((defun ,(struct-info-report-function s)
		    ,@(cdr (struct-info-report-function-lambda-expr s)))))
	,@(let ((copier-name (copier-name s)))
	    (if (null copier-name)
		nil
		`((defun ,copier-name (s)
		    (,(struct-copy-function s) s)))))
	',(struct-info-name s)
	))))

;;; Condition system stuff
(defun report-function (condition-type)
  (struct-info-report-function (lookup-structure-info condition-type)))

(defun make-function (condition-type)
  (struct-info-constructor (lookup-structure-info condition-type)))

(defmacro defforeign (names in/out)
  (multiple-value-bind (name foreign-name)
      (if (listp names)
	  (values (first names) (second names))
	  (values names (string-downcase
			 (symbol-name names))))
    (multiple-value-bind (ins outs in-types out-types)
	(parse-in/out in/out)
      `(progn
	(define-foreign-function
	  ',name ',foreign-name ',ins ',outs ',in-types ',out-types)
	(defun ,name ,ins (,name ,@ins))))))

(defvar *package* nil)			; ug - INTERN needs this below

(defmacro def-c-struct (name &rest fields)
  (let* ((info (parse-c-struct-info name fields))
	 (fields (loop for f in (c-struct-info-slots info)
		       collect (c-struct-slot-name f))))
    `(progn
      (define-c-structure ,info)
      (defun ,(intern (format nil "MAKE-~A" name)) ,(cons '&key fields)
	(let ((s (new-fptr (c-code ,(format nil "sizeof(struct ~A);"
					       name)))))
	  s))
      (defun ,(intern (format nil "~A-P" name)) (s)
	t)
      ,@(loop for slot in (c-struct-info-slots info)
	 collect
	 ;; HEY! make this defmethod once (c-type foo) works
	 `(defun-inline
	   ,(intern (format nil "~A-~A" name (c-struct-slot-name slot))) (s)
	   (c-struct-ref s ,name ,(c-struct-slot-name slot))))
      ;; HEY! add definers and setf stuff
      ',name)))

  
(defmacro def-c-type-name (name type)
  `(progn
    (define-c-type-name ',name ',type)
    ',name))



