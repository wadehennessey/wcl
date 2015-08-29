;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun lookup-library (lib-name)
  (gethash lib-name *libraries*))

(defun lookup-initialized-library (lib-name)
  (let ((l (lookup-library lib-name)))
    (if (or (null l) (= (length (library-symbol-table l)) 0))
	(read-library-link-info lib-name)
	l)))

(defun set-lookup-library (lib-name value)
  (setf (gethash lib-name *libraries*) value))

(defsetf lookup-library set-lookup-library)

(defun library-info-file (lib-name)
  (format nil
	  "~A/lib/lib~A.info"
	  (executable-root-directory)  ;; used to be *root-directory*
	  (string-downcase lib-name)))

(defun library-unix-name (library)
  (format nil 
	  "~Alib~A.so"
	  (library-directory library)
	  (string-downcase (library-name library))
	  (library-version library)))

(defun write-library-info (lib)
  (warn "Writing ~A library information" (library-name lib))
  (let ((*package* *compiler-package*)
	(pinfo (library-procedure-info lib))
	(c-type-info (library-c-type-info lib)))
    (with-open-file (output (library-info-file (library-name lib))
			    :direction :output)
      (format output "~D~%" (+ (hash-table-count pinfo)
			       (hash-table-count c-type-info)))
      (write-c-type-info c-type-info output)
      (write-procedure-info pinfo output)
      (write-library-proclaims lib output)
      (format output "~S~%" (library-version lib))
      (format output "~S~%" (library-init-thunk lib))
      (write-library-symbols lib output)
      lib)))

(defun write-library-symbols (lib output)
  (let ((symbol-table (library-symbol-table lib)))
    (loop for app-package being the array-elements of symbol-table
	  do (maphash
	      #'(lambda (sym appsym)
		  (format output "~S ~S ~S ~S~%"
			  sym
			  (let ((v (application-symbol-value appsym)))
			    (if (eq v *unbound*)
				'unbound
				v))
			  (application-symbol-function appsym)
			  (application-symbol-flags appsym)))
	      (application-package-symbols app-package)))))

(defun write-library-proclaims (lib output)
  (let ((*package* *compiler-package*)
	(*print-circle* nil)
	(*print-array* t)
	(*print-structure* t)
	(proclaims (library-proclaims lib)))
    (format output "~D~%" (length proclaims))
    (dolist (p proclaims)
      (format output ":proclaim ~S~%" p))))

(defun read-library-link-info (name)
  (warn "Reading ~A library linking information" name)
  (let ((*package* *compiler-package*))
    (with-open-file (input (library-info-file name))
      ;; Discard procedure info - let the compiler read that if needed.
      ;; GAG! READ doesn't eat newlines, so we eat them explicitly
      (dotimes (i (prog1 (read input t) (read-line input)))
	(read-line input))
      ;; Discard proclaims info
      (dotimes (i (prog1 (read input t) (read-line input)))
	(read-line input))
      (let* ((version (read input t))
	     (init-thunk (read input t))
	     (symbol-table (new-symbol-table))
	     (lib (or (lookup-library name) (define-library name version))))
	(setf (library-init-thunk lib) init-thunk)
	(setf (library-symbol-table lib) symbol-table)
	(read-library-symbols symbol-table input)
	(setf (lookup-library name) lib)
	lib))))

(defun read-library-symbols (symbol-table input)
  (loop for symbol = (read input nil input)
	until (eq symbol input)
	do (let ((value (read input t))
		 (function (read input t))
		 (flags (read input t))
		 (app-sym (intern-application-symbol-in-symbol-table
			   symbol symbol-table)))
	     (setf (application-symbol-value app-sym)
		   (if (eq value 'unbound) *unbound* value))
	     (setf (application-symbol-function app-sym) function)
	     (setf (application-symbol-flags app-sym) flags))))

(defun read-library-proclaims (input)
  (let ((*package* *compiler-package*))
    (dotimes (i (read input t))
      (read input t)			; discard :proclaims
      (proclaim-w (eval (read input t))))))

(defun read-all-libraries-compiler-info ()
  (dolist (l *default-libraries*)
    (read-library-compiler-info l)))

(defun read-library-compiler-info (lib-name)
  (let ((procedure-info-file (library-info-file lib-name)))
    (when (probe-file procedure-info-file)
      (warn "Loading ~A library compiler information" (symbol-name lib-name))
      (with-open-file (input procedure-info-file)
	(read-procedure-info input)
	(read-library-proclaims input)))
    lib-name))

(defun define-library (name version
			    &key
			    (directory (format nil "~A/lib/" *root-directory*))
			    lisp-files
			    other-object-files
			    (init-thunk (format nil "init_library_~A"
						(string-downcase name))))
  (setf (gethash name *libraries*)
	(make-library :name name
		      :version version
		      :directory directory
		      :lisp-files lisp-files
		      :init-thunk init-thunk
		      :symbol-table (new-symbol-table)
		      :procedure-info (new-function-info-table)
		      :c-type-info (make-hash-table :size 300)
		      :proclaims nil
		      :other-object-files other-object-files)))

(defun library-all-object-files (library)
  (append (mapcar #'binary-pathname (library-lisp-files library))
	  (mapcar #'binary-pathname  (library-other-object-files library))))

