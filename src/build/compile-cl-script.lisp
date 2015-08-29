;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.


(in-package "LISP")

(when (= *target-bits-per-word* 64)
  (setf (config-cc-optimizer-level *default-config*) 0)
  (setf (config-cc-optimizer-level *fastest-config*) 0)
  (setf (config-cc-optimizer-level *debug-library-config*) 0)
  (setf (config-cc-optimizer-level *standard-library-config*) 0)
  (setf (config-cc-optimizer-level *delivery-library-config*) 0)
)

(load "compute-root.lisp")
(load "library-definitions.lisp")

;;; hack to make new loop package visible
(list-all-packages)
(print "ansi-loop")


(make-cl-library)
(make-eval-bin)
(make-compiler-library)
(make-development-bin)



