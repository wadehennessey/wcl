;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defmacro defprimitive (name in/out &body body)
  (multiple-value-bind (ins outs in-types out-types)
      (parse-in/out in/out)
    `(eval-when (compile load eval)
      (define-primitive
	  ',name ',ins ',outs
	  (mapcar #'c-type-name->c-type-object ',in-types)
	  (mapcar #'c-type-name->c-type-object ',out-types)
	  #'(lambda ,ins ,@body)))))

(defmacro emit-c (string &rest args)
  `(format *c-stream* ,string ,@args))

(defmacro emit-lc (tree string &rest args)
  `(progn (emit-source-line ,tree)
    (format *c-stream* ,string ,@args)))

(defmacro emit-k (string &rest args)
  `(format *k-stream* ,string ,@args))

(defmacro emit-win (string &rest args)
  `(format *win-stream* ,string ,@args))

;;; other macros are already in the library or in com/cross/macros.lisp
