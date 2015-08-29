;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun lmain ()
  (format t
	  ";;; WCL ~A Development Environment for x86 Linux version ~A on ~A~%"
	  *wcl-version* (os-version) (machine-instance))
  (format t ";;; Common Lisp ~A library built at ~A~%"
	  *cl-version* *cl-build-date*)
  (load-init-file)
  (repl))

