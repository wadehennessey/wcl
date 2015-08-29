;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defmacro iterate (name var-vals &body body)
  `(labels ((,name ,(mapcar #'first var-vals)
	     ,@body))
    (,name ,@(mapcar #'second var-vals))))

;;; HACK! use DEFVAR?
(when (or (not (boundp '*type-macro-expanders*))
	  (null *type-macro-expanders*))
  (setf *macro-expanders* (make-hash-table :size 200 :test #'eq))
  (setf *compiler-macro-expanders* (make-hash-table :size 200 :test #'eq))
  (setf *type-macro-expanders* (make-hash-table :size 100))
  (setf *setf-methods* (make-hash-table :size 100 :test #'eq)))

(defvar *macroexpand-hook* #'funcall
  "Function used to invoke macro expansion functions")

(defstruct macro-env
  macros
  symbol-macros)

(defstruct basic-macro
  original-arg-list
  expansion-function)

(defstruct (macro (:include basic-macro)))

(defstruct (pcl-walker-macro (:include macro))
  info)

(defstruct (compiler-macro (:include basic-macro)))

(defstruct (type-macro (:include basic-macro)))

(defmacro letf ((accessor-form new-value) &body body)
  (let ((old-value (gensym "OLD-VALUE-")))
    `(let ((,old-value ,accessor-form))
      (unwind-protect (progn (setf ,accessor-form ,new-value)
			     ,@body)
	(setf ,accessor-form ,old-value)))))

(defmacro key-list-iterate (name (ivar list-form &optional done-form)
				 var-init-pairs
				 &body body)
  (let ((iteration-label
	 (gensym (concatenate 'string (symbol-name name) "-")))
	(remaining-list (gensym "REMAINING-LIST-"))
	(vars (mapcar #'first var-init-pairs))
	(vals (mapcar #'second var-init-pairs)))
    `(macrolet ((,name (&key ,@(mapcar #'(lambda (var)
					   `(,var ',var))
				       vars))
		 (list ',iteration-label
		       (list 'cdr ',remaining-list)
		       ,@vars)))
      (labels ((,iteration-label ,(cons remaining-list vars)
		 (if (null ,remaining-list)
		     ,done-form
		     (let ((,ivar (car ,remaining-list)))
		       ,@body))))
	(,iteration-label ,list-form ,@vals)))))

(deftype lambda-expr ()
  '(satisfies lambda-expr?))

