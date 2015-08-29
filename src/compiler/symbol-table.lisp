;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun new-symbol-table ()
  (make-array 50 :fill-pointer 0 :adjustable t))

(defun new-structure-table ()
  (make-hash-table :size 200 :test #'eq))

(defun clear-symbol-table (symbol-table)
  (dotimes (i (length symbol-table))
    (clrhash (application-package-symbols (aref symbol-table i))))
  (setf (fill-pointer symbol-table) 0)
  symbol-table)

(defun find-application-package (package symbol-table)
  (dotimes (i (length symbol-table))
    (let ((entry (aref symbol-table i)))
      (when (eq (application-package-host-package entry)
		package)
	(return entry)))))

(defun find-or-make-application-package (package symbol-table)
  (or (find-application-package package symbol-table)
      (let* ((host-package package)
	     (new (make-application-package
		   :host-package host-package
		   :symbol-array-c-name
		   (lisp->c-name "package_symbols_"
				 (package-name host-package)
				 0)
		   :symbols (make-hash-table :size 6000 :test #'eq))))
	(vector-push-extend new symbol-table)
	new)))

(defun find-application-symbol-in-symbol-table (symbol symbol-table)
  (let ((app-package (find-application-package (symbol-package symbol)
					       symbol-table)))
    (if (null app-package)
	nil
	(multiple-value-bind (app-symbol found?)
	    (gethash symbol (application-package-symbols app-package))
	  (if found? app-symbol nil)))))

(defun find-application-symbol-in-libraries (symbol libraries)
  (dolist (lib libraries)
    (let ((app-symbol (find-application-symbol-in-symbol-table
		       symbol (library-symbol-table lib))))
      (unless (null app-symbol)
	(return app-symbol)))))

(defun find-application-symbol (symbol libraries symbol-table)
  (or (find-application-symbol-in-libraries symbol libraries)
      (find-application-symbol-in-symbol-table symbol symbol-table)))

(defun intern-application-symbol (symbol libraries symbol-table)
  (let ((app-sym (find-application-symbol symbol libraries symbol-table)))
    (if (null app-sym)
	(make-new-application-symbol-in-symbol-table symbol symbol-table)
	app-sym)))

(defun intern-application-symbol-in-symbol-table (symbol symbol-table)
  (or (find-application-symbol-in-symbol-table symbol symbol-table)
      (make-new-application-symbol-in-symbol-table symbol symbol-table)))

(defun make-new-application-symbol-in-symbol-table (symbol symbol-table)
  (let* ((package (symbol-package symbol))
	 (app-symbol (make-application-symbol
		      :sym symbol
		      :value (if (eq package *keyword-package*)
				 symbol
				 *unbound*)))
	 (app-pkg (find-or-make-application-package package symbol-table)))
    (setf (gethash symbol (application-package-symbols app-pkg))
	  app-symbol)))

(defun intern-application-symbol-used-as-data (symbol libraries symbol-table)
  (let ((s (intern-application-symbol symbol libraries symbol-table)))
    (setf (application-symbol-used-as-data? s) t)
    s))

(defun set-application-symbol-flag (s flag-position)
  (setf (application-symbol-flags s)
	(logior (application-symbol-flags s) (ash 1 flag-position))))

(defun set-application-symbol-value (app-sym vlabel flag)
  (unless (eq (application-symbol-value app-sym) *unbound*)
    (warn "Multiple symbol values for ~A" (application-symbol-sym app-sym)))
  (set-application-symbol-flag app-sym flag)
  (setf (application-symbol-value app-sym) vlabel))

(defun set-application-symbol-function (app-sym flabel)
  (unless (null (application-symbol-function app-sym))
    (warn "Multiple symbol functions for ~A" (application-symbol-sym app-sym)))
  (setf (application-symbol-function app-sym) flabel))

(defun setup-application-symbols (libraries symbol-table structures winfiles
					    &optional library)
  (loop
   with inits = nil
   with symbol-fixups = nil
   for winfile in winfiles do
   (with-open-file (input (merge-pathnames ".wcl" winfile))
     (let ((*package* *package*))
       (read-line input)		; discard "/*"
       (loop
	for cmd = (read input nil input)
	until (eq cmd :end)
	do
	(let ((symbol (read input)))
	  (case cmd
	    (:init (push symbol inits))
	    (:sym (intern-application-symbol-used-as-data
		   symbol libraries symbol-table))
	    (:comment nil)
	    (:version nil)		; HEY! check it
	    (:proclaim (unless (null library)
			 (push symbol (library-proclaims library))))
	    (:pinfo
	     (if (null library)
		 (read-line input)
		 (let* ((table (library-procedure-info library))
			(info (or (gethash symbol table)
				  (setf (gethash symbol table)
					(make-proc-info :name symbol)))))
		   (read-procedure-info-line info input))))
	    (:finfo
	     (if (null library)
		 (read-line input)
		 (let* ((table (library-procedure-info library))
			(info (or (gethash symbol table)
				  (setf (gethash symbol table)
					(make-foreign-info :name symbol)))))
		   (read-foreign-info-line info input))))
	    (:end-package-info nil)
	    (:package (eval symbol))	; symbol is really a pkg cmd
	    (:c-type (let ((type (read input)))
		       (unless (null library)
			 (setf (gethash symbol (library-c-type-info library))
			       type))))
	    (:structure
	     (read-line input)		; discard fluid-predicate-name-c-name
	     (add-application-structure symbol structures))
	    (t
	     (let ((argument (read input)))
	       (let ((existing-app-sym
		      (find-application-symbol-in-libraries
		       symbol libraries)))
		 (if (null existing-app-sym)
		     (let ((new-app-sym
			    (intern-application-symbol-in-symbol-table
			     symbol symbol-table)))
		       (case cmd
			 (:sv (set-application-symbol-value
			       new-app-sym argument special-symbol-flag))
			 (:sc (set-application-symbol-value
			       new-app-sym argument constant-symbol-flag))
			 (:sf (set-application-symbol-function
			       new-app-sym argument))
			 (:sm (set-application-symbol-function
			       new-app-sym argument)
			      (set-application-symbol-flag
			       new-app-sym macro-symbol-flag))))
		     ;; Cannot statically init other library symbols, so
		     ;; we must collect code to alter them at run time.
		     (push (symbol-init->c-code cmd symbol argument)
			   symbol-fixups))))))))))
   finally (return (values (nreverse inits) ; preserve init order!
			   (nreverse symbol-fixups)))))

(defun symbol-init->c-code (cmd symbol argument)
  (let ((symbol-c-name (lisp->c-symbol-name symbol))
	(s (make-string-output-stream)))
    (unwind-protect
	 (ecase cmd
	   ((:sv :sc)
	    (let* ((*k-stream* s)
		   (data-ref (emit-data argument))
		   (data-decl (get-output-stream-string s))
		   (flag (case cmd
			   (:sv special-symbol-flag)
			   (:sc constant-symbol-flag))))
	      (format nil "{extern SYMBOL ~A; ~A UPDATE_VAR(~A,~A,~D);~%"
		      symbol-c-name data-decl symbol-c-name data-ref flag)))
	   ((:sf :sm)
	    (let* ((label (emit-application-proc argument s))
		   (proc-decl (get-output-stream-string s)))
	      (case cmd
		(:sf (format nil
			     "{extern SYMBOL ~A; ~A UPDATE_FUNC(~A,~A);}"
			     symbol-c-name proc-decl symbol-c-name label))
		(:sm (error "Fix linker macro update"))))))
      (close s))))

(defun add-application-structure (info table)
  (let ((name (struct-info-name info)))
    (when (gethash name table)
      (warn "Multiple structure definitions for ~A" name))
    (setf (gethash name table) info)
    (let ((include-info (gethash (struct-info-include info) table)))
      (unless (null include-info)
	(pushnew (struct-info-name info)
		 (struct-info-children include-info))))))

(defun transitive-structure-children (info table)
  (if (null (struct-info-children info))
      nil
      (loop for c in (struct-info-children info)
	    appending (cons c (transitive-structure-children (gethash c table)
							     table)))))

(defun application-structure-predicate (info table)
  (let* ((name (struct-info-name info))
	 (children (transitive-structure-children info table)))
    `(defun ,(predicate-name info) (s)
      ,(if (struct-info-dynamic? info)
	   `(,(fluid-predicate-name info) s)
	   `(and (structurep s)
	     ,(if (null children)
		  `(%eq (structure-type s) ',name)
		  `(find-eq/simple-vector (structure-type s)
		    ,(coerce (cons name children) 'vector))))))))

(defun emit-application-proc (name stream)
  (if (null name)
      "ubf_procedure"
      (let ((label (genstring "p")))
	;; apply and funcall are already declared in lisp.h 
	(unless (or (string= name "p_lsp_APPLY")
		    (string= name "p_lsp_FUNCALL"))
	  (format stream "~%extern LP ~A();~%" name))
	(format stream "MAKE_PROCEDURE(~A,~A);~%" label name)
	label)))

(defun emit-application-symbol (s)	
  (let* ((vcell (application-symbol-value s))
	 (vcell-constant (cond ((eq vcell *unbound*) nil)
			       ;; LABEL stuff is obsolete???
			       ;; ((label-p vcell) (label-name vcell))
			       (t (emit-lref vcell))))
	 (proc (emit-application-proc (application-symbol-function s)
				      *k-stream*)))
    (let* ((sym (application-symbol-sym s))
	   (name (emit-lref (symbol-name sym)))
	   (hash-code (emit-lref (sxhash (application-symbol-sym s)))))
      (emit-k "~%MAKE_SYMBOL(~A,~A,NIL,~A,NIL,LREF(~A),~A,~D);~%"
	      (application-symbol-c-name s)
	      (if (null vcell-constant)
		  "((LP) UBV_MARKER)"
		  vcell-constant)
	      name
	      proc
	      hash-code
	      (application-symbol-flags s)))))

(defun link-application-symbol? (app-symbol)
 (or *link-every-symbol?*
     (application-symbol-used-as-data? app-symbol)))

(defun emit-symbol-table-symbols (symbol-table)
  (loop for app-package being the array-elements of  symbol-table
	do (maphash #'(lambda (sym app-sym)
			(declare (ignore sym))
			(when (link-application-symbol? app-sym)
			  (emit-application-symbol app-sym)))
		    (application-package-symbols app-package))))

(defun emit-symbol-table-symbol-arrays (symbol-table)
  (loop for app-package being the array-elements of symbol-table
	do
	(progn (emit-k "~%static unsigned long ~A[] = {~%"
		       (application-package-symbol-array-c-name app-package))
	       (maphash #'(lambda (sym app-sym)
			    (declare (ignore sym))
			    (when (link-application-symbol? app-sym))
			    (emit-k "LREF(~A),~%"
				    (application-symbol-c-name app-sym)))
			(application-package-symbols app-package))
	       (emit-k "0};~%"))))

(defun emit-symbol-table-registration-code (symbol-table)
  (loop for app-package being the array-elements of symbol-table
	do (let ((package (application-package-host-package app-package)))
	     ;; HEY! This is a hack to deal with uninterned symbols.
	     ;; probably need something better...
	     (unless (null package)
	       (emit-k "register_symbols(~S,~A);~%"
		       (package-name package)
		       (application-package-symbol-array-c-name
			app-package))))))

(defun emit-symbol-table (libraries symbol-table)
  (format *k-stream* "~%#include \"lisp.h\"~%~%")
  ;; add new sym found to new table, then merge new into main table.
  ;; then emit arrays.
  (let ((*emit-symbol-data-function*
	 #'(lambda (symbol)
	     (unless (find-application-symbol symbol libraries symbol-table)
	       (break "Symbol ~S loses!" symbol)))))
    (emit-symbol-table-symbols symbol-table))
  (emit-symbol-table-symbol-arrays symbol-table))

  



