;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun comf (file &key
		  (output-file (merge-pathnames ".o" file))
		  (c-file (merge-pathnames ".wcl" output-file))
		  (verbose *compile-verbose*)
		  (print *compile-print*)
		  (config *config*)
		  (pic? *pic?*)
		  only-to-c?)
  (initialize-compiler)
  (let ((*package* *package*)
	(*const-labels* (make-hash-table :test #'equal))
	(*pic?* pic?)
	(*config* config)
	(*print-base* 10)
	(*print-gensym* t)
	(*input-stream-line-numbers?* (config-lisp-line-numbers? config))
	(*new-function-info* (new-function-info-table 200))
	(*referenced-c-info* nil)
	(input-file (merge-pathnames file ".lisp"))
	(code-file (tmp-file-name ".p"))
	(k-file (tmp-file-name ".k"))
	(package-file (tmp-file-name ".pkg"))
	(win-file (tmp-file-name ".w")))
    (unwind-protect
	 (progn
	   (with-open-file (input-stream input-file)
	     (with-open-file (*package-stream* package-file :direction :output)
	       (with-open-file (*c-stream* code-file :direction :output)
		 (with-open-file (*win-stream* win-file :direction :output)
		   (with-open-file (*k-stream* k-file :direction :output)
		     (let ((*compile-file-pathname* (pathname input-stream))
			   (*readtable* (if (config-lisp-line-numbers?
					     *config*)
					    *line-number-readtable*
					    *readtable*))
			   (*source-table*
			    (make-hash-table :size 300 :test #'equal))
			   (*external-procs*
			    (make-hash-table :size 300 :test #'eq)))
		       (when verbose
			 (format *standard-output* "~&Compiling file ~S~%"
				 *compile-file-pathname*))
		       (comf-begin)
		       (comf-loop input-stream print)
		       (comf-finish)))))))
	   (append-files c-file package-file win-file k-file code-file))
      (del-files package-file win-file k-file code-file))
    (if only-to-c?
	(pathname c-file)
	(progn (invoke-c-compiler (namestring c-file)
				  (namestring output-file))
	       (when verbose
		 (format *standard-output* "~&Wrote object file ~S~%"
			 output-file))
	       output-file))))

(defun comf-begin ()
  (emit-configuration-info)
  (emit-k "#include \"lisp.h\"~%~%"))

(defun comf-loop (input-stream print)
  (loop for form = (progn
		     (clrhash *source-table*)
		     (read input-stream nil input-stream))
	until (eq form input-stream)
	;; HEY! make print option print something more meaningful
	when print do (princ "Compiling top-level form")
	for init = (comf-top-level-form form)
	unless (null init)
	append init into thunk-body
	finally
	(let ((init-name (file-init-thunk-name input-stream)))
	  (unless (null thunk-body)
	    (compile-define-function
	     (macroexpand `(defun ,init-name ()
			      ,@thunk-body)))
	    (emit-win ":init ~S~%" init-name))
	  (maphash #'(lambda (lisp-name c-name)
		       (declare (ignore lisp-name))
		       (unless (eq c-name :done)
			 (emit-k "extern LP ~A();~%" c-name)))
		   *external-procs*))))

(defun comf-finish ()
  (emit-k "~%~%")
  (format *package-stream* ":end-package-info 0~%")
  (emit-referenced-c-definitions)
  (when (config-lisp-line-numbers? *config*)
    (emit-k "#line 1 \"~A\"~%"
	    (namestring *compile-file-pathname*)))
  (write-procedure-info *new-function-info* *win-stream*)
  (emit-win ":end~%*/~%~%"))

(defun emit-configuration-info ()
  (format *package-stream* "/*~%")
  (format *package-stream* ":comment \"Compiled at ")
  (print-time :stream *package-stream*)
  (format *package-stream* "\"~%")
  (format *package-stream* ":comment \"Compiler Configuration: ~A\"~%"
	  (config-name *config*))
  (format *package-stream* ":comment \"Machine Configuration: ~A\"~%"
	  (machine-name *target-machine*))
  (format *package-stream* ":comment \"cc command: ~A\"~%"
	  (basic-cc-string))
  (format *package-stream* ":version ~A~%" 0))

(defun comf-top-level-form (form)
  (if (atom form)
      nil
      (let ((mexp (macroexpand form))
	    (*current-line* (if (config-lisp-line-numbers? *config*)
				(source-line form)
				nil)))
	(case (car mexp)
	  (define-function (compile-define-function mexp))
	  (define-variable (compile-define-var mexp))
	  (define-macro (compile-define-macro mexp))
	  (define-type (compile-define-type mexp))
	  (define-compiler-macro-1 (compile-define-compiler-macro-1 mexp))
	  (define-structure (compile-top-level-define-structure mexp))
	  (define-c-structure (compile-top-level-define-c-structure mexp))
	  (define-c-type-name (compile-top-level-define-c-type-name mexp))
	  (define-foreign-function
	      (compile-top-level-define-foreign-function mexp))
	  (define-setf (compile-top-level-define-setf mexp))
	  (proclaim (compile-top-level-proclaim mexp))
	  (progn (compile-top-level-progn mexp))
	  (eval-when (compile-top-level-eval-when mexp))
	  (in-package (compile-top-level-package-related-form mexp))
	  (add-winfo (compile-top-level-add-winfo mexp))
	  (t (list mexp))))))

(defun compile-top-level-add-winfo (form)
  (emit-win (second form))
  (emit-win "~%")
  nil)

(defun compile-top-level-package-related-form (form)
  (format *package-stream* ":package ~S ~%" form)
  (eval form)
  nil)

(defun compile-top-level-progn (form)
  (loop for x in (cdr form)
	for init = (comf-top-level-form x)
	unless (null init)
	append init into thunk-body
	finally (return thunk-body)))

(defun compile-top-level-eval-when (form)
  (destructuring-bind (ignore when . body) form
    (declare (ignore ignore))
    (when (member 'compile when)
      (eval `(progn ,@body)))
    (if (member 'load when)
	(comf-top-level-form `(progn ,@body))
	nil)))


(defun compile-top-level-define-c-type-name (form)
  (destructuring-bind (define-c-type-name (q1 name) (q2 type)) form
    (declare (ignore define-c-type-name q1 q2))
    (emit-win ":c-type ~S ~S~%" name type)
    (define-c-type-name name type)
    nil))

(defun compile-top-level-define-foreign-function (form)
  (eval form)
  nil)

(defun compile-define-function (form)
  (destructuring-bind (define-function (q1 name) kind
			(q3 in-types) (q4 out-types)
			(q5 body)
			function-with-type-checks
			function) form
    (declare (ignore define-function q1 q2 q3 q4 q5))
    (when (lookup-macro-expander name *macro-expanders* nil)
      (remove-macro-expander name)
      (warn "Redefining macro ~A as a function" name))
    (let ((real-function
	   (if (and (eq kind :defmethod)
		    (config-full-type-checking? *config*))
	       function-with-type-checks
	       function)))
      (let ((flabel (com-1 real-function)))
	(unless (null flabel)
	  (add-proc-definition
	   name body function *compile-file-pathname*)
	  (when (eq kind :defmethod)
	    (proclaim-ftype-info name in-types out-types))
	  (emit-win ":sf ~S ~S~%" name flabel)))
      nil)))

(defun compile-define-macro (form)
  (eval form)	
  (destructuring-bind (define-macro
			  (q1 name)
			  (f1 function))
      form
    (declare (ignore define-macro q2 f1 q1))
    (let ((flabel (com-1 `(named-macro-function ,name ,function))))
      (emit-win ":sm ~S ~S~%" name flabel)
      (list `(define-macro ',name #',name)))))

(defun compile-define-type (form)
  (eval form)
  (list form))

(defun compile-define-compiler-macro-1 (form)
  (eval form)	
  (list form))

(defun compile-top-level-define-setf (form)
  (eval form)
  ;; this sometimes breaks kcl
  (list form))

(defun compile-top-level-define-structure (form)
  (let ((info (second form))
	(*print-structure* t))
    (let ((*print-array* t))
      (emit-win ":structure ~S ~S~%"
		info
		(lisp->c-proc-name (fluid-predicate-name info))))
    (define-structure info)
    (list form)))


(defun compile-top-level-define-c-structure (form)
  (let ((info (second form))
	(*print-structure* t)
	(*print-array* t))
    (emit-win ":c-type ~S ~S~%" (c-struct-info-name info) info)
    (define-c-structure info)
    nil))

(defun compile-define-var (form)
  (destructuring-bind (i0 (i1 name) init-form doc-string type) form
    (declare (ignore i0 i1 doc-string))
    (ecase type
      ((:var :parameter) (proclaim-special-variable name))
      (:constant (proclaim-constant-variable name init-form)))
    (if (simple-constant? init-form)
	(progn (emit-win "~S ~S ~S~%"
			 (if (eq type :constant) :sc :sv) name init-form)
	       nil)
	(list form))))

(defun simple-constant? (x)
  ;; HEY! add vector and quoted list support. Add general quoted object stuff.
  ;; Have to emit all subobjects of the constant (i.e symbols)
  ;; as well as the constant. 
  (or (stringp x) (numberp x)))

(defun com-1 (form)
  (let* ((*proc-chain* nil)
	 (tree (analyze form)))
    (if (null tree)
	(warn "Not emitting c code for ~A" (form-name form))
	(if (top-level-proc-p tree)
	    (let ((*emitting-proc?* nil))
	      (back-end tree))
	    (error "Only expect top-level-procs at top-level")))))

(defun back-end (tree)
  (when (config-beta? *config*)
    (beta tree nil))
  (improve tree)
  (emit-code tree))

(defun compile-top-level-proclaim (form)
  (destructuring-bind (ignore-1 decl-spec) form
    (declare (ignore ignore-1))
    (proclaim-w (eval decl-spec))
    (emit-win ":proclaim ~S~%" decl-spec)
    nil))

(defun proclaim-w (decl-spec)
  (let ((decl (car decl-spec))
	(spec (cdr decl-spec)))
    (case decl
      (optimize (loop for (quality value) in spec
		      do (case quality
			   (speed (setf *config*
					(ecase value
					  ((0 1 2)  *default-config*)
					  (3 *fastest-config*))))
			   (safety nil))))
      (declaration (loop for decl in spec
			 do (pushnew decl *ok-foreign-declarations*)))
      (special (loop for special in spec
		     do (proclaim-special-variable special)))
      (inline (loop for name in spec do (proclaim-inline-function name)))
      (notinline (loop for name in spec
		       do (proclaim-notinline-function name)))
      (type (loop with type = (first spec)
		  for var in (rest spec) do
		  (proclaim-variable-type var type)))
      ;; Punt for now. Maybe add ANSI interp later.
      (function (let ((function-names spec))
		  (when (listp (second function-names))
		    (warn "Ignoring obsolete Cltl1 style declaration: ~S"
			  decl-spec))))
      (ftype (destructuring-bind ((function in-types out-type) . names)
		 spec
	       (declare (ignore function))
	       (let ((out-types (if (listp out-type) ; (values ...) ?
				    (cdr out-type) 
				    (list out-type))))
		 (loop for name in names
		       do (proclaim-ftype-info name in-types out-types)))))
      (t (if (member decl *standard-type-specifier-symbols*)
	     ;; HEY! record type info
	     nil
	     (unless (member decl *ok-foreign-declarations*)
	       (error "Unknown declaration ~A" decl-spec)))))
    t))

(defun get-variable-info (name)
  (gethash name *variable-info*))

(defun get-or-create-variable-info (name)
  (or (get-variable-info name)
      (setf (gethash name *variable-info*)
	    (make-variable-info :name name))))

(defun proclaim-variable-type (variable type)
  (setf (variable-info-type (get-or-create-variable-info variable))
	type))

(defun special-var-p (var)
  (lookup-special-decl var (lex-env-decls *env*)))

(defun compiler-warn (string &rest args)
  (incf *analysis-errors*)
  (when (= *analysis-errors* 1)
    (format *error-output*
	    "~%The following errors were detecting in the ~A:~% "
	    (form-name (lex-env-outermost-form *env*))))
  (format *error-output* "~8T~A~%" (apply #'format nil string args))
  (when *break-on-compiler-warn?* (break))
  nil)

(defun w-warn (string &rest args)
  (apply #'format *error-output* string args)
  nil)

(defun form-name (form)
  (or (and (listp form)
	   (case (first form)
	     (named-function (format nil "function ~A" (second form)))
	     (define-variable (format nil "global variable~A" (second form)))))
      (let ((*print-level* 3)
	    (*print-length* 3))
	(format nil "Top Level Form ~A" form))))

(defun initialize-compiler ()
  (initialize-function-info-table)
  (unless *compiler-initialized?*
    (setf *target-machine* (default-target-machine))
    (read-all-libraries-compiler-info)
    ;; Add primitive, compiler-macro, and compiler-method inits.
    (initialize-function-methods)
    ;; HEY! avoid gcc -O<N> compilation bugs that mess up clx programs
    ;;(when (member :clx *features*)
    ;;  (setf (config-cc-optimizer-level *default-config*) 0))
    (setf *compiler-initialized?* t)))

(defun default-target-machine ()
  #.(let ((machine-type (processor+os->machine-type
			 (installation-parameter "PROCESSOR")
			 (installation-parameter "OPERATING_SYSTEM")))
	  (cc (installation-parameter "CC")))
      (case machine-type
	(:linux-pc *linux-gcc*))))


(defun tmp-file-name (format-string &rest args)
  (pathname (format nil "~A/~D-~D~A"
		    (tmpdir)
		    (getpid)
		    (incf *tmp-file-counter*)
		    (apply #'format nil format-string args))))

(defun del-files (&rest files)
  (loop for f in files
	when (probe-file f)
	do (delete-file f)))

(defun del-derived-files (&rest files)
  (loop for file in files
	do (del-files file
		      (merge-pathnames ".o" file)
		      (merge-pathnames ".wcl" file))))

(defun append-files (dest &rest sources)
  (shell (format nil "cat ~{ ~A ~} > ~A"
		 (mapcar #'namestring sources)
		 (namestring dest))))

(defun file-init-thunk-name (pathname)
  (gentemp (format nil "~A_INIT" (string-upcase (pathname-name pathname)))))

(defun compile-file (&rest args)
  (apply #'comf args))

(defun compile (name &optional definition)
  (let* ((real-name (if (null name) (gentemp "ANONYMOUS") name))
	 (def (if (null definition)
		  (function-lambda-expression name)
		  (setf (get real-name :function-definition)
			`(defun ,real-name ,@(cdr definition))))))
    (compile-and-load-def def)
    (if (null name)
	(symbol-function real-name)
	name)))

(defun compile-and-load-def (def)
  (let ((tmp-file (tmp-file-name "compile.lisp")))
    (unwind-protect
	 (progn (with-open-file (output tmp-file :direction :output)
		  (print def output))
		(load (compile-file tmp-file :verbose nil)
		      :verbose nil))
      ;; Leave all files around for debugging purposes
      ;; (del-files (make-pathname :defaults tmp-file :type "o"))
      nil)))

(defun find-root-directory ()
  (let ((cl-lib-file (format nil "libcl.so.~A" *cl-version*)))
    (dolist (dir (unix->lisp-path-list (getenv "LD_LIBRARY_PATH")))
      (let* ((raw-name (namestring dir))
	     (dir-name (if (string= #\/ (aref raw-name (1- (length raw-name))))
			   raw-name
			   (concatenate 'string raw-name "/"))))
	(when (probe-file (merge-pathnames cl-lib-file dir-name))
	  (if (char= (aref dir-name 0) #\/)
	      (return (pathname
		       (concatenate
			'string
			(subseq dir-name
				0
				(position #\/ dir-name
					  :end (1- (length dir-name))
					  :from-end t))
			"/")))
	      (warn "Need full pathname in LD_LIBRARY_PATH instead of ~S"
		    dir-name)))))))

(pushnew :compiler *features*)



