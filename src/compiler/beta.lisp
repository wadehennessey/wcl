;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun beta (tree env)
  (unless (null tree)
    (typecase tree
      (seq (beta-seq tree env))
      (scope-control-transfer (beta-scope-control-transfer tree env))
      (unwind-protect (beta-unwind-protect tree env))
      (var-ref (beta-var-ref tree env))
      (var-def (beta-var-def tree env))
      (mvalues (beta-values tree env))
      (if (beta-if tree env))
      (switch (beta-switch tree env))
      (function-call (beta-function-call tree env))
      (control-point (beta-control-point tree env))
      (t tree))))

(defun beta-list (l env)
  (loop for rest on l
	do (setf (car rest) (beta (car rest) env))
	finally (return l)))

(defun beta-body (body env)
  (if (atom body)
      (beta body env)
      (beta-list body env)))

(defun beta-seq (tree env)
  (typecase tree
    (named-local (beta-named-local tree env))
    (progn (beta-progn tree env))
    (t (when (values-seq-p tree)
	 (beta-list (values-seq-values tree) env))
       (when (scope-seq-p tree)
	 (setf (scope-seq-control-point tree)
	       (beta (scope-seq-control-point tree) env)))
       (setf (seq-body tree) (beta-body (seq-body tree) env))
       tree)))

(defun beta-progn (tree env)
  (let ((beta-body (beta-body (seq-body tree) env)))
    (cond ((atom beta-body) beta-body)
	  ((= (length beta-body) 1) (car beta-body))
	  (t (setf (seq-body tree) beta-body)
	     tree))))

(defun beta-named-local (tree env)
  (let ((vars (named-local-vars tree))
	(values (beta-list (named-local-values tree) env))
	(body (seq-body tree)))
    (multiple-value-bind (new-vars new-vals new-env)
	(collect-beta-pairs vars values nil nil env)
      (if (null new-vars)
	  (let ((beta-body (beta body (acons (first (named-local-vars tree))
					     (first values)
					     new-env))))
	    (setf (code-out-type beta-body) (code-out-type tree))
	    tree)
	  (progn (setf (named-local-vars tree) new-vars)
		 (setf (named-local-values tree) new-vals)
		 (setf (seq-body tree) (beta-body body new-env))
		 tree)))))

(defun collect-beta-pairs (orig-vars orig-vals new-vars new-vals env)
  (if (null orig-vars)
      (values (nreverse new-vars) (nreverse new-vals) env)
      (let ((var (car orig-vars))
	    (val (car orig-vals)))
	(unless (null val)		; HEY! How can this be null???
	  (propagate-out-type var val))
	(if (and (beta-value? val)
		 (variable-var-p var)	; avoid fvar subst for now...
		 ;; More than one subst means we need to increment
		 ;; the tmp var count.
		 ;; also need to keep part of the orig so
		 ;; that attributes like tail?, etc. are distinct
		 ;; for each occurence.
		 (<= (var-num-refs var) 1)
		 (= (var-num-defs var) 0)
		 ;; Don't try to subst through closure boundries
		 (eq (var-extent var) :dynamic))
	    (collect-beta-pairs (cdr orig-vars)
				(cdr orig-vals)
				new-vars
			         new-vals
				(acons var val env))
	    (collect-beta-pairs (cdr orig-vars)
				(cdr orig-vals)
				(cons var new-vars)
				(cons val new-vals)
				env)))))

(defun propagate-out-type (var val)
  (let ((var-type (var-definite-type var))
	(val-type (code-out-type val)))
    (when (and (= (var-num-defs var) 0)
	       (eq var-type t))
      (setf (var-definite-type var) val-type))))

(defun beta-value? (val)
  (or (constant-p val)
      (and (var-ref-p val)
	   (= (var-num-defs (var-ref-var val)) 0))))
#|
      ;; OOPS! Consider this LETF expansion:
      ;;((LAMBDA (OLD-VALUE-2652)
      ;;         (UNWIND-PROTECT (PROGN (SETF (CAR A) X)
      ;;                                (PRINT A))
      ;;                         (SETF (CAR A) OLD-VALUE-2652)))
      ;;  (CAR A))
      ;; Disable primtive call subst until we can be careful enough
      ;; to avoid doing the beta-subst in the code above.
      (and (primitive-call-p val)
	   (dolist (arg (primitive-call-args val) t)
	     (unless (beta-value? arg) (return nil))))
|#

(defun beta-values (tree env)
  (beta-list (mvalues-args tree) env)
  tree)

(defun beta-var-ref (tree env)
  ;; Update out-type of ref in case we found out new info about var type.
  (setf (code-out-type tree) (var-definite-type (var-ref-var tree)))
  (let ((entry (assoc (var-ref-var tree) env :test #'eq)))
    (if (null entry)
	tree
	(let ((new (cdr entry)))
	  (setf (code-tail? new) (code-tail? tree))
	  (merge-out-type new (code-out-type tree))
	  new))))

(defun beta-var-def (tree env)
  (setf (var-def-value tree) (beta (var-def-value tree) env))
  tree)

(defun beta-function-call (tree env)
  (when (unnamed-call-p tree)
    (setf (unnamed-call-function-form tree)
	  (beta (unnamed-call-function-form tree) env)))
  (beta-list (function-call-args tree) env)
  tree)

(defun beta-if (tree env)
  (setf (if-test tree) (beta (if-test tree) env))
  (setf (if-then tree) (beta (if-then tree) env))
  (setf (if-else tree) (beta (if-else tree) env))
  tree)

(defun beta-switch (tree env)
  (setf (branch-test tree) (beta (branch-test tree) env))
  (setf (switch-consequents tree) (beta-list (switch-consequents tree) env))
  (setf (switch-default tree) (beta (switch-default tree) env))
  tree)

(defun beta-scope-control-transfer (tree env)
  (setf (scope-control-transfer-send-value tree)
	(beta (scope-control-transfer-send-value tree) env))
  (setf (scope-control-transfer-destination-point tree)
	(beta (scope-control-transfer-destination-point tree) env))
  tree)

(defun beta-control-point (tree env)
  (typecase tree
    (dynamic-scope-control-point
       (setf (dynamic-scope-control-point-tag-name tree)
	     (beta (dynamic-scope-control-point-tag-name tree) env)))
    (dynamic-tag-control-point
     (setf (dynamic-tag-control-point-tag-name tree)
	   (beta (dynamic-tag-control-point-tag-name tree) env))))
  tree)

(defun beta-unwind-protect (tree env)
  (setf (unwind-protect-cleanup-form tree)
	(beta (unwind-protect-cleanup-form tree) env))
  (setf (unwind-protect-protected-form tree)
	(beta (unwind-protect-protected-form tree) env))
  tree)



