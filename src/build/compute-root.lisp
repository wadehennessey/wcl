;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; Assumes we're in the build dir!
(defun compute-root-directory ()
  (cd "../../")
  (let ((r (namestring (pwd))))
    (cd "src/build")
    (if (char= (schar r (1- (length r))) #\/)
	r
	(format nil "~A/" r))))
	       
(defparameter *root-directory*  (compute-root-directory))
