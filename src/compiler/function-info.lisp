;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defmacro define-compiler-method ((name new-function) in/out
				  &optional (transform '#'identity))
  (multiple-value-bind (ins outs in-types out-types)
      (parse-in/out in/out)
    `(add-compiler-method ',name ',ins ',outs
      ',in-types ',out-types ',new-function ,transform)))

(defmacro define-meta-eval (function-spec arg-types)
  `(add-meta-eval-method ',function-spec ',arg-types))

(defun add-meta-eval-method (function-spec arg-types)
  (multiple-value-bind (function-name meta-eval-function)
      (if (listp function-spec)
	  (values (first function-spec) (second function-spec))
	  (values function-spec function-spec))
    (let ((info (get-or-create-proc-info function-name)))
      (setf (function-info-meta-eval-function info) meta-eval-function)
      (setf (function-info-meta-eval-arg-types info) arg-types)
      function-name)))

(defun add-compiler-method (name ins outs in-types out-types
				 new-function transform)
  (let ((info (get-or-create-proc-info name)))
    (loop for method in (function-and-method-info-methods info)
	  when (and (equal (compiler-method-in-types method) in-types)
		    (equal (compiler-method-out-types method) out-types))
	  do (return (setf (function-and-method-info-methods info)
			   (delete method
				   (function-and-method-info-methods info)
				   :test #'eq))))
    (push (make-compiler-method :name name
				:ins ins
				:outs outs
				:in-types in-types
				:out-types out-types
				:new-function new-function
				:transform transform)
	  (function-and-method-info-methods info))
    name))
 
(defun get-proc-info (name)
  (initialize-function-info-table)
  (or (and (not (null *new-function-info*))
	   (gethash name *new-function-info*))
      (gethash name *primary-function-info*)))

(defun new-function-info-table (&optional (size 3000))
  (make-hash-table :size size))

(defun initialize-function-info-table ()
  (when (null *primary-function-info*)
    (setf *primary-function-info* (new-function-info-table 5000))))

(defun get-or-create-proc-info (name)
  (let ((info (or (get-proc-info name)
		  (setf (gethash name *primary-function-info*)
			(make-proc-info :name name)))))
    (unless (null *new-function-info*)
      (setf (gethash name *new-function-info*) info))
    info))

(defun add-proc-definition (name body function source)
  (declare (ignore source))
  (let ((entry (get-or-create-proc-info name)))
    (unless (foreign-info-p entry)	; Ignore Lisp def of foreign funcs
      (delete-undefined-function name)
      (setf (proc-info-defined? entry) t)
      (destructuring-bind (lambda lambda-list . actual-body) (third function)
	(declare (ignore lambda actual-body))
	(when (proc-info-inline? entry)
	  (setf (proc-info-lambda-expr entry)
		`(lambda ,lambda-list ,@body)))
	(setf (proc-info-ins entry) lambda-list)
	(setf (proc-info-source-file entry) nil)))) ; save some space
  name)

(defun proclaim-ftype-info (name in-types out-types)
  (let ((entry (get-or-create-proc-info name)))
    (setf (proc-info-in-types entry) in-types)
    (setf (proc-info-out-types entry) out-types)
    name))

(defun proclaim-inline-function (name)
  (setf (proc-info-inline? (get-or-create-proc-info name)) t)
  name)

(defun proclaim-notinline-function (name)
  (setf (proc-info-inline? (get-or-create-proc-info name)) nil)
  name)

			  
(defun delete-proc-info (name)
  (remhash name *primary-function-info*)
  (unless (null *new-function-info*) 
    (remhash name *new-function-info*)))

(defun validate-function-call (call)
  call)

(defun add-undefined-function (name caller)
  (push (gethash name *undefined-functions*) caller))

(defun delete-undefined-function (name)
  (remhash name *undefined-functions*))

(defun add-function-call-info (caller callee)
  caller callee
  nil)

(defun clear-undefined-functions ()
  (clrhash *undefined-functions*))

(defun list-undefined-functions ()
  (maphash #'(lambda (function callers)
	       (format t "~A was called by ~{~A ~}~%"  function callers))
	   *undefined-functions*))

(defun write-procedure-info (procedure-info output)
  (let ((*package* *compiler-package*)
	(*print-circle* nil)
	(*print-array* t)
	(*print-structure* t))
    (maphash #'(lambda (key value)
		 (typecase value
		   (proc-info
		    (format output ":pinfo ~S ~S ~S ~S ~S ~S ~S ~S ~S~%"
			    key
			    (proc-info-ins value)
			    (proc-info-outs value)
			    (proc-info-in-types value)
			    (proc-info-out-types value)
			    (proc-info-lambda-expr value)
			    (proc-info-source-file value)
			    (proc-info-inline? value)
			    (proc-info-defined? value)))
		   (foreign-info
		    (format output ":finfo ~S ~S ~S ~S ~S ~S~%"
			    (foreign-info-name value)
			    (foreign-info-foreign-name value)
			    (foreign-info-ins value)
			    (foreign-info-outs value)
			    (foreign-info-in-types value)
			    (foreign-info-out-types value)))))
	     procedure-info)))

(defun write-c-type-info (c-type-info output) 
  (let ((*package* *compiler-package*)
	(*print-circle* nil)
	(*print-array* t)
	(*print-structure* t))
    ;; Have to write structure defs out first
    (maphash #'(lambda (name type)
		 (when (c-struct-info-p type)
		   (format output ":c-type ~S ~S~%" name type)))
	     c-type-info)
    ;; Then write all other named types
    (maphash #'(lambda (name type)
		 (unless (c-struct-info-p type)
		   (format output ":c-type ~S ~S~%" name type)))
	     c-type-info)))

(defun read-procedure-info (input)
  (let ((*package* *compiler-package*))
    (dotimes (i (read input t))
      (let ((type (read input t))
	    (name (read input t)))
	(ecase type
	  (:pinfo (setf (gethash name *primary-function-info*)
			(read-procedure-info-line
			 (get-or-create-proc-info name)
			 input)))
	  (:finfo (setf (gethash name *primary-function-info*)
			(read-foreign-info-line (make-foreign-info :name name)
						input)))
	  (:c-type (let ((type (read input t)))
		     (if (c-struct-info-p type)
			 (define-c-structure type)
			 (define-c-type-name name type)))))))))


(defun read-procedure-info-line (info input)
  (let ((*package* *compiler-package*))
    (setf (proc-info-ins info) (read input t))
    (setf (proc-info-outs info) (read input t))
    (setf (proc-info-in-types info) (read input t))
    (setf (proc-info-out-types info) (read input t))
    (setf (proc-info-lambda-expr info) (read input t))
    (setf (proc-info-source-file info) (read input t))
    (setf (proc-info-inline? info) (read input t))
    (setf (proc-info-defined? info) (read input t))
    info))

(defun read-foreign-info-line (info input)
  (let ((*package* *compiler-package*))
    (setf (foreign-info-foreign-name info) (read input t))
    (setf (foreign-info-ins info) (read input t))
    (setf (foreign-info-outs info) (read input t))
    (let ((in-types (read input t))
	  (out-types (read input t)))
      (setf (foreign-info-in-types info) in-types) 
      (setf (foreign-info-out-types info) out-types)
      (setf (foreign-info-in-type-objects info)
	    (mapcar #'c-type-name->c-type-object in-types))
      (setf (foreign-info-out-type-objects info)
	    (mapcar #'c-type-name->c-type-object out-types))
      info)))

(defun define-foreign-function (name foreign-name ins outs in-types
				     out-types)
  (let ((old-info (get-proc-info name)))
    (setf (gethash name *primary-function-info*)
	  (make-foreign-info
	   :name name
	   :foreign-name foreign-name
	   :ins ins
	   :outs outs
	   :in-types in-types
	   :out-types out-types
	   :in-type-objects (mapcar #'c-type-name->c-type-object in-types)
	   :out-type-objects (mapcar #'c-type-name->c-type-object out-types)
	   ;; Be sure to preserve method info!
	   :methods (if (null old-info)
			nil
			(function-and-method-info-methods old-info)))))
  name)

(defun define-primitive (name ins outs in-types out-types emitter)
  (setf (gethash name *primary-function-info*)
	(make-primitive-info
	 :name name
	 :ins ins
	 :outs outs
	 :in-types in-types
	 :out-types out-types
	 :emitter emitter))
  name)
