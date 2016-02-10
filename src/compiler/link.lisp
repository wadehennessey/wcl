;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; Link a shared library.
;;; Only create symbols NOT found in other libraries.
(defun link-library (lib-name &key 
			      other-lib-names
			      (predicates-file (tmp-file-name "preds.lisp") p?)
			      (data-file (tmp-file-name "data.wcl") d?))
  (let ((*pic?* t))
    (unwind-protect
	 (let ((*link-start-time* (get-universal-time))
	       (library (lookup-library lib-name)))
	   (create-library-aux-files
	    lib-name other-lib-names predicates-file data-file)
	   (ld-shared-library library predicates-file data-file)
	   library)
      ;; If caller supplies and aux file name, assume they want to keep it.
      (unless p? (del-derived-files predicates-file))
      (unless d? (del-derived-files data-file)))))

(defun create-library-aux-files (lib-name other-lib-names
					predicates-file data-file)
					
  (let* ((library (lookup-library lib-name))
	 (other-libraries (mapcar #'lookup-initialized-library
				  other-lib-names))
	 (symbol-table (clear-symbol-table (library-symbol-table library)))
	 (structures (new-structure-table))
	 (*const-labels* (make-hash-table :size 5000 :test #'equal)))
    (clrhash (library-procedure-info library))
    (clrhash (library-c-type-info library))
    (setf (library-proclaims library) nil)
    (link-msg "Reading symbol information")
    (multiple-value-bind (lisp-init-thunks symbol-fixups)
	(setup-application-symbols other-libraries symbol-table structures
				   (library-lisp-files library) library)
      (compile-predicates-file
       predicates-file other-libraries symbol-table structures)
      (compile-library-data-file data-file other-libraries
				 symbol-table symbol-fixups
				 (library-init-thunk library)
				 lisp-init-thunks)
      (write-library-info library)
      lib-name)))
    

(defun link-executable (files &key
			      output
			      (lib-names *default-libraries*)
			      (dynamic-size 8192)
			      (static-size 512)
			      (main-function 'user::lmain)
			      (predicates-file (tmp-file-name
						"preds.lisp") p?)
			      (data-file (tmp-file-name "data.wcl") d?)
			      (foreign-libs '("c" "m" "dl")))
  (unwind-protect
       (let* ((*link-start-time* (get-universal-time))
	      (file-list (if (listp files) files (list files)))
	      (output (or output (make-pathname
				  :defaults (first file-list)
				  :type nil)))
	      (libraries (mapcar #'lookup-initialized-library
				 lib-names))
	      (all-libs
	       (append (mapcar #'(lambda (l)
				   (string-downcase (library-name l)))
			       libraries)
		       foreign-libs)))
	 (create-executable-aux-files
	  file-list libraries dynamic-size static-size main-function
	  predicates-file data-file)
	 (ld-executable output file-list predicates-file data-file all-libs)
	 output)
    (unless p? (del-derived-files predicates-file))
    (unless d? (del-derived-files data-file))))

(defun create-executable-aux-files (files libraries dynamic-size static-size
					  main-function
					  predicates-file data-file)
  (let* ((symbol-table (new-symbol-table))
	 (structures (new-structure-table))
	 (*const-labels* (make-hash-table :size 5000 :test #'equal)))
    (link-msg "Reading symbol information")
    (multiple-value-bind (lisp-init-thunks symbol-fixups)
	(setup-application-symbols libraries symbol-table structures files)
      (compile-predicates-file
       predicates-file libraries symbol-table structures)
      (compile-executable-data-file  data-file libraries
				     symbol-table symbol-fixups
				     lisp-init-thunks
				     main-function
				     dynamic-size static-size))))


(defun compile-predicates-file (predicates-file libs symbol-table structures)
  (with-open-file (output predicates-file :direction :output)
    (maphash #'(lambda (name info)
		 (declare (ignore name))
		 (write (application-structure-predicate info structures)
			:stream output
			:array t)
		 (terpri output))
	     structures))
  (link-msg "Compiling predicates file")
  (comf predicates-file)
  ;; Add new symbols created in preds file to symbol-table
  (setup-application-symbols
   libs symbol-table structures (list predicates-file)))


;;;    lib_init_thunk (c-code)
;;;      <c code register_symbols for each package in lib>
;;;      <symbol-fixups>
;;;      call-each-lib-file-init-thunk
(defun compile-library-data-file (data-file
				  other-libraries
				  symbol-table symbol-fixups
				  thunk-name library-lisp-thunks)
  (link-msg "Writing data file")
  ;; Setup stream for EMIT-DATA
  (with-open-file (*k-stream* data-file :direction :output)
    (emit-symbol-table other-libraries symbol-table)
    (emit-k "~%~A()~%{~%" thunk-name)
    (emit-symbol-table-registration-code symbol-table)
    (emit-k "~{ ~A~% ~}~%" symbol-fixups)
    (dolist (lisp-thunk library-lisp-thunks)
      (emit-k "~A(0);~%" (lisp->c-proc-name lisp-thunk)))
    (emit-k "}~%"))
  (link-msg "Compiling data file")
  (invoke-c-compiler data-file))

;;;   main (c-code)
;;;     start_initialization(sizes);
;;;     <c code register_symbols for each package in app>
;;;     <symbol-fixups>
;;;     call-each-lib-init-thunk 
;;;     call-each-app-file-init-thunk  [LISP]
;;;     (catch (main-func))  [LISP]
(defun compile-executable-data-file (data-file
				     libraries
				     symbol-table symbol-fixups
				     application-lisp-thunks
				     main-function
				     dynamic-size static-size)
  (link-msg "Writing data file")
  ;; Setup stream for EMIT-DATA
  (with-open-file (*k-stream* data-file :direction :output)
    (emit-symbol-table libraries symbol-table)
    (emit-k "main(argc,argv) int argc; char *argv[]; {~%")
    (emit-k "start_initialization(argc,argv,~D,~D);~%"
	    dynamic-size static-size)
    (emit-symbol-table-registration-code symbol-table)
    (emit-k "~{ ~A~% ~}~%" symbol-fixups)
    (dolist (lib libraries)
      (emit-k "~A();~%" (library-init-thunk lib)))
    (dolist (lisp-thunk application-lisp-thunks)
      (emit-k "~A(0);~%" (lisp->c-proc-name lisp-thunk)))
    (emit-k "p_lsp_START_2DAPPLICATION(1,LREF(~A));~%"
	    (lisp->c-symbol-name main-function))
    (emit-k "}~%"))
  (link-msg "Compiling data file")
  (invoke-c-compiler data-file))

(defun ld-shared-library (library predicates-file data-file)
  (link-msg "Linking shared library")
  (shell (format nil
		 "ld -dy -G -o ~A ~{ ~A ~} ~A ~A -lgmp -lc -lm -ldl -lnsl~%"
		 (library-unix-name library)
		 (library-all-object-files library)
		 (binary-pathname predicates-file)
		 (binary-pathname data-file))))

(defun ld-executable (output-file files preds-file data-file unix-libs)
  (invoke-linker
       (format nil
	       "~A -o ~A ~A ~A ~{~A ~} -L~A/lib ~{-l~A ~}"
	       (if *profile?* "-p " "")
	       (namestring output-file)
	       (namestring (merge-pathnames ".o" preds-file))
	       (namestring (merge-pathnames ".o" data-file))
	       (mapcar #'binary-pathname files)
	       ;; *root-directory*
	       *root-directory*
	       unix-libs))
  (link-msg "done")
  output-file)

(defun invoke-c-compiler (file &optional
			       (output (merge-pathnames ".o" file)))
  (let ((status
	 (funcall
	  #'shell
	  (format nil "~A  -c -I~A/include -o ~A ~A"
		  (basic-cc-string)
		  (namestring *root-directory*)
		  (namestring output)
		  (namestring file)))))
    (unless (successful-status? status)
      (error "Failed to compile file: ~S, status: ~D" file status))
    status))

(defun basic-cc-string ()
  (let ((c-compiler (machine-c-compiler *target-machine*)))
    (concatenate 'string
		 (c-compiler-command c-compiler)
		 " "
		 (if (config-cc-debug-info? *config*)
		     (c-compiler-debug-switch c-compiler)
		     nil)
		 " "
		 (if (or (not (config-cc-debug-info? *config*))
			 (c-compiler-debug-optimized? c-compiler))
		     (svref (c-compiler-optimizer-switches c-compiler)
			    (config-cc-optimizer-level *config*))
		     "")
		 " "
		 (if *pic?*
		     (c-compiler-position-independent-code-switch
		      (machine-c-compiler *target-machine*))
		     ""))))

(defun invoke-linker (cmd)
  (let ((status
	 (funcall #'shell
		  (concatenate 'string
			       (machine-linker-command *target-machine*)
			       " " cmd " "
			       (machine-link-libraries *target-machine*)))))
    (unless (successful-status? status)
      (error "Failed to link: ~S, status: ~D" cmd status))
    status))

(defun successful-status? (s)
  (= s 0))

(defun elapsed-link-time ()
  (- (get-universal-time) *link-start-time*))

(defun link-msg (msg)
  (format t "~A~%" msg))

(defun c-pathname (name)
  (merge-pathnames ".wcl" name))

(defun binary-pathname (name)
   (merge-pathnames ".o" name))
