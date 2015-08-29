;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun lookup-structure-info (name)
  (gethash name *structure-info*))

(defun define-structure (s)
  (add-structure-info s)
  (if *delay-structure-defs?*
      (push s *delayed-structure-defs*)
      (complete-structure-def s))
  (struct-info-name s))

(defun complete-structure-def (s)
  (loop for slot being the elements of (all-slots s) using (index i)
	do (define-setf (accessor-name s slot) (updater-name s slot))))

(defun add-structure-info (s)
  ;; HEY! This is a hack to copy structure info from static space
  ;; to the heap.
  (setf (gethash (struct-info-name s) *structure-info*) (copy-struct-info s))
  (unless (null (struct-info-include s))
    (pushnew (struct-info-name s)
	     (struct-info-children
	      (lookup-structure-info (struct-info-include s))))))

;;; Bletch, cross compiler redefines this...
(defun defstruct-package (symbol)
  (symbol-package symbol))

(defun inherited-structure-name-p (expected actual)
  (structure-predicate-1 	
   (lookup-structure-info expected) actual))

(defun structure-predicate-1 (struct-info actual-type)
  (let ((children (struct-info-children struct-info)))
    (if (null children)
	nil
	(loop for child in children
	      when (or (eq actual-type child)
		       (structure-predicate-1
			(lookup-structure-info child) actual-type))
	      do (return t)
	      finally (return nil)))))

(defun parent-structure (s)
  (let ((info (lookup-structure-info s)))
    (and info (struct-info-include info))))

(defun structure-children (name)
  (struct-info-children (lookup-structure-info name)))

(defun all-current-structure-children (name)
  (let ((info (lookup-structure-info name)))
    (if (null (struct-info-children info))
	nil
	(loop for c in (struct-info-children info)
	      appending (cons c (all-current-structure-children c))))))

(defun default-constructor-name (s)
  (intern (format nil "MAKE-~A" (symbol-name (struct-info-name s)))
	  (defstruct-package (struct-info-name s))))

(defun fixed-arg-constructor-name (s constructor-arg-slot-names)
  (let ((c (struct-info-constructor s)))
    (intern (format nil "~A/~D"
		    (symbol-name c)
		    (length constructor-arg-slot-names))
	    (defstruct-package (struct-info-name s)))))

(defun constructor-arg-list (s all-slots)
  (if (eq (struct-info-constructor-arg-list s) :default)
      `(&key ,@(loop for slot in all-slots
		collect (list (struct-slot-name slot)
			 (struct-slot-initial-value slot))))
      (struct-info-constructor-arg-list s)))

(defun accessor-name (s slot)
  (intern (format nil "~A~A"
		  (struct-info-conc-name s)
		  (symbol-name (struct-slot-name slot)))
	  (defstruct-package (struct-info-name s))))

;;; HEY! This and the FLUID-<pred> symbols shouldn't be visible to the user
(defun updater-name (s slot)
  (intern (format nil "SET-~A~A!@$"	; hack hack
		  (struct-info-conc-name s)
		  (symbol-name (struct-slot-name slot)))
	  (defstruct-package (struct-info-name s))))

(defun predicate-name (s)
  (or (struct-info-predicate s)
      (setf (struct-info-predicate s)
	    (intern (format nil "~A-P" (symbol-name (struct-info-name s)))
		    (defstruct-package (struct-info-name s))))))

(defun fluid-predicate-name (s)
  (intern (format nil "FLUID-~A!@$"	; hack hack
		  (symbol-name (predicate-name s)))
	  (defstruct-package (struct-info-name s))))
  
(defun copier-name (s)
  (struct-info-copier s))

(defun printer-name (s)
  (intern (format nil "PRINT-FUNCTION-FOR-~A-ANONYMOUS"
		  (symbol-name (struct-info-name s)))
	  (defstruct-package (struct-info-name s))))

(defun reporter-name (s)
  (intern (format nil "REPORT-FUNCTION-FOR-~A-ANONYMOUS"
		  (symbol-name (struct-info-name s)))
	  (defstruct-package (struct-info-name s))))

(defun all-slots (s)
  (let ((all (default-all-slots s)))
    (loop for override in (struct-info-inherited-slot-overrides s)
	  do (let* ((name (struct-slot-name override))
		    (l (member name all :key #'struct-slot-name))
		    (old (car l)))
	       (setf (car l)
		     (make-struct-slot
		      :name name
		      :initial-value (struct-slot-initial-value override)
		      ;; HEY! add error checks to the next 2
		      :type (struct-slot-type override)
		      :read-only? (or (struct-slot-read-only? old)
				      (struct-slot-read-only? override))))))
    all))

(defun default-all-slots (s)
  (let ((include (struct-info-include s)))
    (if (null include)
	(struct-info-slots s)
	(append (let ((info (lookup-structure-info include)))
		  (if (null info)
		      (error "Cannot include structure ~A in structure ~A ~
                              because ~A is not defined"
			     include (struct-info-name s) include)
		      (default-all-slots info)))
		(struct-info-slots s)))))
    
(defun fill-in-defstruct-option-info (info given-options)
  (multiple-value-bind (name opts named?)
      (etypecase given-options
	(symbol (values given-options nil nil))
	(list (values (car given-options)
		      (remove :named (cdr given-options))
		      (find :named (cdr given-options)))))
    (flet ((option (o)
	     (unless (null (assoc o (cdr (member o opts :key #'car))))
	       (error "DEFSTRUCT option ~A appears more than once" o))
	     (second (assoc o opts))))
      (setf (struct-info-name info) name)
      (setf (struct-info-conc-name info)
	    (if (assoc :conc-name opts)
		(let ((conc-name (option :conc-name)))
		  (if (null conc-name)
		      ""
		      conc-name))
		(format nil "~A-" (symbol-name name))))
      (setf (struct-info-predicate info) (option :predicate))
      (let ((include-spec (assoc :include opts)))
	(unless (null include-spec)
	  (setf (struct-info-include info) (second include-spec))
	  (setf (struct-info-inherited-slot-overrides info)
		(mapcar #'parse-struct-slot-info (cddr include-spec)))))
      (let ((constructor-spec (assoc :constructor opts)))
	(if (null constructor-spec)
	    (setf (struct-info-constructor info)
		  (default-constructor-name info))
	    (progn
	      (setf (struct-info-constructor info) (second constructor-spec))
	      (let ((rest (cddr constructor-spec)))
		(unless (null rest)
		  (setf (struct-info-constructor-arg-list info)
			(car rest)))))))
      (setf (struct-info-copier info) 
	    (if (assoc :copier opts)
		(option :copier)
		(intern (format nil "COPY-~A" (symbol-name
					       (struct-info-name info)))
			(defstruct-package (struct-info-name info)))))
      (let ((printer (option :print-function)))
	(etypecase printer
	  (symbol (setf (struct-info-print-function info)
			(if (null printer)
			    'default-write-structure
			    printer)))
	  (list (setf (struct-info-print-function info) (printer-name info))
		(setf (struct-info-print-function-lambda-expr info) printer))))
      (let ((reporter (option :report-function)))
	(etypecase reporter
	  (symbol (setf (struct-info-report-function info) reporter))
	  (list (setf (struct-info-report-function info) (reporter-name info))
		(setf (struct-info-report-function-lambda-expr info)
		      reporter))))
      (setf (struct-info-type info)
	    (or (let* ((given-type (or (option :type) 'structure))
		       ;; HEY! Use type-macroexpand here to
		       ;; determine actual-type?
		       (actual-type given-type))
		  (if (member actual-type '(structure list vector))
		      actual-type
		      (error "~A is not a legal structure type" given-type)))))
      ;; HEY! warn that :named and :initial-offset can only be used with :type
      (setf (struct-info-named? info)
	    (or named? (eq (struct-info-type info) 'structure)))
      (setf (struct-info-initial-offset info)
	    (or (option :initial-offset) 0))
      (setf (struct-info-dynamic? info) (option :dynamic))
      info)))

(defun parse-struct-slot-info (given-slot)
  (let ((slot (etypecase given-slot
		(symbol (list given-slot))
		(list given-slot))))
    (make-struct-slot
     :name (first slot)
     :initial-value (second slot)
     :type (or (second (member :type slot)) 't)
     :read-only? (eq (third slot) :read-only))))

(defun struct-new-structure-function (s)
  (case (struct-info-type s)		
    (structure 'new-structure)
    (list 'make-list)
    (vector 'make-array)))

(defun struct-set-slot-function (s)
  (case (struct-info-type s)
    (structure 'set-structure-elt)
    (list 'set-elt/list)
    (vector 'set-svref)))

(defun struct-get-slot-function (s)
  (case (struct-info-type s)
    (structure 'structure-elt)
    (list 'elt/list)
    (vector 'svref)))

(defun struct-copy-function (s)
  (case (struct-info-type s)
    (structure 'copy-structure)
    (list 'copy-list)
    (vector 'copy-vector)))


(defun struct-type-check (s)
  (if (struct-info-named? s)
      (struct-info-name s)
      (struct-info-type s)))

(defun-inline set-structure-elt (s n value)
  (%set-structure-elt s n value))

(defun-inline structure-elt (s i)
  (%structure-elt s i))

;;; Condition stuff needed to expand define-condition
(defun parse-keyword-pairs (list keys)
  (do ((l list (cddr l))
       (k '() (list* (cadr l) (car l) k)))
      ((or (null l) (not (member (car l) keys)))
       (values (nreverse k) l))))

#| modify or zap to allow inherited slot init
(defun parse-new-and-used-slots (slots parent-type)
  (let ((new '()) (used '()))
    (dolist (slot slots)
      (if (slot-used-p (car slot) parent-type)
	  (push slot used)
	  (push slot new)))
    (values new used)))
  
(defun slot-used-p (slot-name type)
  (cond ((eq type 'condition) nil)
	((not type)
	 (error "The type ~S does not inherit from condition." type))
	((assoc slot-name (slots type)))
	(t (slot-used-p slot-name (parent-type type)))))

|#
