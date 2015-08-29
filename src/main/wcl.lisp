;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun lmain ()
  (setf *root-directory* (or (probe-file #.(compute-root-directory))
			     (find-root-directory)))
  (format
   t
   ";;; WCL ~A Development Environment for ~A Linux version ~A on ~A~%"
   *wcl-version*
   (if (= (host_bits_per_word) 64) "x86_64" "x86_32")
   (os-version)
   (machine-instance))
  (format t ";;; Common Lisp ~A library built at ~A~%"
	  *cl-version* *cl-build-date*)
  (format t ";;; Compiler ~A library built at ~A~%"
	  *compiler-version* *compiler-build-date*)
  (load-init-file)
  (repl))



