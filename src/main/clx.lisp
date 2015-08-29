;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun lmain ()
  (setf *root-directory* (or (probe-file #.(compute-root-directory))
			     (find-root-directory)))
  (setf *default-libraries* '(:cl :clx))
  (format
   t
   ";;; WCL ~A Development Environment for x86 Linux version ~A on ~A~%"
   *wcl-version* (os-version) (machine-instance))
  (format t ";;; Common Lisp ~A library built at ~A~%"
	  *cl-version* *cl-build-date*)
  (format t ";;; Compiler ~A library built at ~A~%"
	  *compiler-version* *compiler-build-date*)
  (format t ";;; CLX ~A library built at ~A~%"
	  xlib::*clx-version* xlib::*clx-build-date*)
  (load-init-file)
  (repl))


