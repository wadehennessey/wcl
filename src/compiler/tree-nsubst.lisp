;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun tree-nsubst (new old tree)
  (if (eq old tree)
      (progn (setf (code-tail? new) (code-tail? old))
	     new)
      (unless (null tree)
	(typecase tree
	  (seq (tree-nsubst-seq new old tree))
	  (scope-control-transfer (tree-nsubst-scope-control-transfer
				   new old tree))
	  (unwind-protect (tree-nsubst-unwind-protect new old tree))
	  (var-def (tree-nsubst-var-def new old tree))
	  (mvalues (tree-nsubst-values new old tree))
	  (if (tree-nsubst-if new old tree))
	  (switch (tree-nsubst-switch new old tree))
	  (function-call (tree-nsubst-function-call new old tree))
	  (control-point (tree-nsubst-control-point new old tree))
	  (t tree)))))

(defun tree-nsubst-list (new old l)
  (loop for rest on l
	do (setf (car rest) (tree-nsubst new old (car rest)))
	finally (return l)))

(defun tree-nsubst-seq (new old tree)
  (when (values-seq-p tree)
    (tree-nsubst-list new old (values-seq-values tree)))
  (when (scope-seq-p tree)
    (setf (scope-seq-control-point tree)
	  (tree-nsubst-seq new old (scope-seq-control-point tree))))
  (let ((body (seq-body tree)))
    (setf (seq-body tree)
	  (if (atom body)
	      (tree-nsubst new old body)
	      (tree-nsubst-list new old body)))
    tree))

(defun tree-nsubst-values (new old tree)
  (tree-nsubst-list new old (mvalues-args tree))
  tree)

(defun tree-nsubst-var-def (new old tree)
  (setf (var-def-value tree) (tree-nsubst new old (var-def-value tree)))
  tree)

(defun tree-nsubst-function-call (new old tree)
  (when (unnamed-call-p tree)
    (setf (unnamed-call-function-form tree)
	  (tree-nsubst new old (unnamed-call-function-form tree))))
  (tree-nsubst-list new old (function-call-args tree))
  tree)

(defun tree-nsubst-if (new old tree)
  (setf (if-test tree) (tree-nsubst new old (if-test tree)))
  (setf (if-then tree) (tree-nsubst new old (if-then tree)))
  (setf (if-else tree) (tree-nsubst new old (if-else tree)))
  tree)

(defun tree-nsubst-switch (new old tree)
  (setf (branch-test tree) (tree-nsubst new old (branch-test tree)))
  (setf (switch-consequents tree)
	(tree-nsubst-list new old (switch-consequents tree)))
  (setf (switch-default tree) (tree-nsubst new old (switch-default tree)))
  tree)

(defun tree-nsubst-scope-control-transfer (new old tree)
  (setf (scope-control-transfer-send-value tree)
	(tree-nsubst new old (scope-control-transfer-send-value tree)))
  (setf (scope-control-transfer-destination-point tree)
	(tree-nsubst new old (scope-control-transfer-destination-point tree)))
  tree)


(defun tree-nsubst-control-point (new old tree)
  (typecase tree
    (dynamic-scope-control-point
     (setf (dynamic-scope-control-point-tag-name tree)
	   (tree-nsubst new old (dynamic-scope-control-point-tag-name tree))))
    (dynamic-tag-control-point
     (setf (dynamic-tag-control-point-tag-name tree)
	   (tree-nsubst new old (dynamic-tag-control-point-tag-name tree)))))
  tree)

(defun tree-nsubst-unwind-protect (new old tree)
  (setf (unwind-protect-cleanup-form tree)
	(tree-nsubst new old (unwind-protect-cleanup-form tree)))
  (setf (unwind-protect-protected-form tree)
	(tree-nsubst new old (unwind-protect-protected-form tree)))
  tree)


