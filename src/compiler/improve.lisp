;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun improve (tree &optional dead?)
  (unless (null tree)
    (typecase tree
      (seq (improve-seq tree dead?))
      (unwind-protect (improve-unwind-protect tree dead?))
      (var-ref (improve-var-ref tree dead?))
      (var-def (improve-var-def tree))
      (mvalues (improve-values tree dead?))
      (if (improve-if tree dead?))
      (switch (improve-switch tree dead?))
      (function-call (improve-function-call tree dead?))
      (scope-control-transfer (improve-scope-control-transfer tree))
      (control-point (improve-control-point tree))
      (constant (improve-constant tree dead?))
      (t tree))))

(defun improve-constant (tree dead?)
  (if dead? nil tree))

(defun improve-list (l &optional dead? body?)
  (loop with end = (last l)
	for rest on l
	do (setf (car rest) (improve (car rest)
				     (if (eq rest end)
					 dead?
					 body?)))
	finally (return l)))


(defun improve-seq (tree dead?)
  (when (values-seq-p tree)
    (improve-list (values-seq-values tree)))
  (when (scope-seq-p tree)
    (setf (scope-seq-control-point tree)
	  (improve (scope-seq-control-point tree))))
  (let ((body (seq-body tree)))
    (setf (seq-body tree)
	  (if (listp body)
	      (improve-list body dead? t)
	      (improve body)))
    tree))

(defun improve-values (tree dead?)
  (improve-list (mvalues-args tree) dead?)
  tree)

(defun improve-var-ref (tree dead?)
  (if dead? nil tree))

(defun improve-var-def (tree)
  (setf (var-def-value tree) (improve (var-def-value tree)))
  tree)

(defun improve-function-call (tree dead?)
  (when (unnamed-call-p tree)
    (setf (unnamed-call-function-form tree)
	  (improve (unnamed-call-function-form tree))))
  (improve-list (function-call-args tree))
  (let* ((info (function-call-info tree))
	 (meta-eval-arg-types (and info
				   (function-info-meta-eval-arg-types info))))
    (if (and (not (null meta-eval-arg-types))
	     (every #'(lambda (arg type)
			(and (constant-p arg)
			     (typep (constant-data arg) type)))
		    (function-call-args tree)
		    meta-eval-arg-types))
	(let ((const (apply
		      (function-info-meta-eval-function info)
		      (mapcar #'constant-data (function-call-args tree)))))
	  (improve (make-constant :tail? (code-tail? tree)
				  :mv-holder (code-mv-holder tree)
				  :out-type (type-macroexpand (type-of const))
				  :line (code-line tree)
				  :data const)
		   dead?))
	(let ((method (function-call-rewrite-method tree)))
	  (if (null method)
	      ;; Shouldn't we move this outside the method lookup?
	      (if (and dead? (null (function-call-side-effects? tree)))
		  nil
		  tree)
	      (let* ((info (get-or-create-proc-info
			    (compiler-method-new-function method)))
		     (new (etypecase  info
			    (primitive-info
			     (make-primitive-call
			      :tail? (code-tail? tree)
			      :mv-holder (code-mv-holder tree)
			      :out-type (code-out-type tree)
			      :line (code-line tree)
			      :args (function-call-args tree)
			      :info info))
			    (proc-info
			     (make-named-call
			      :name (proc-info-name info)
			      :tail? (code-tail? tree)
			      :mv-holder (code-mv-holder tree)
			      :out-type (code-out-type tree)
			      :line (code-line tree)
			      :args (function-call-args tree)
			      :info info)))))
		(funcall (compiler-method-transform method) new)
		(improve new dead?)))))))
	
(defun function-call-rewrite-method (call)
  (let ((info (function-call-info call)))
    (and (not (null info))
	 (typep call '(or named-call foreign-call))
	 (loop for method in (function-and-method-info-methods info)
	       when (call-matches-method-type-signature? call method)
	       do (return method)
	       finally (return nil)))))

(defun call-matches-method-type-signature? (call method)
  (and (loop for arg in (function-call-args call)
	     for method-arg-type in (compiler-method-in-types method)
	     unless (and (not (null (code-out-type arg))) ; HEY! zap someday
			 (subtypep (code-out-type arg) method-arg-type))
	     do (return nil)
	     finally (return t))
       ;; HEY! only works for single valued calls
       (subtypep (code-out-type call)
		 (first (compiler-method-out-types method)))))

(defun improve-if (tree dead?)
  (let ((test (setf (if-test tree) (improve (if-test tree))))
	(then (setf (if-then tree) (improve (if-then tree) dead?)))
	(else (setf (if-else tree) (improve (if-else tree) dead?))))
    (if (and (null then) (null else))
	(improve test dead?)
	(typecase test
	  (constant (if (null (constant-data test))
			(if-else tree)
			(if-then tree)))
	  ;; (if (if a b c) d e) ==> (if a (if b d e) (if c d e))
	  ;; if false C
#|
	  (if (let ((new-then (make-if :test (if-then test)
				       :then (if-then tree)
				       :else (if-else tree)))
		    (new-else (make-if :test (if-else test)
				       :then (if-then tree)
				       :else (if-else tree))))
		(setf (if-test tree) (if-test test))
		(setf (if-then tree) new-then)
		(setf (if-else tree) new-else)
		(improve-if tree dead?)))
|#
	  (t (let ((leaves (tail-leaves test)))
	       (when (and (= (length leaves) 1)
			  (primitive-call-p (first leaves)))
		 (let* ((call (first leaves))
			(info (function-call-info call))
			(args (function-call-args call)))
		   (cond ((and (eq (function-info-name info) '%eq)
			       (constant-p (second args))
			       (null (constant-data (second args))))
			  ;; Above pattern match depends on the fact
			  ;; that NOT and NULL are inlined as (%EQ X NIL)
			  (rotatef (if-then tree) (if-else tree))
			  (if (eq test call)
			      (setf (if-test tree) (first args))
			      (tree-nsubst (first args) call test))
			  (improve-if tree dead?)) ; try for more improvement
			 ((eq (car (function-info-out-types info))
			      c-type-if-test)
			  (setf (branch-inline-test? tree) t)
			  (setf (code-out-type call)
				c-type-if-test)))))
	       tree))))))


(defun improve-switch (tree dead?)
  (setf (branch-test tree) (improve (branch-test tree)))
  (setf (switch-consequents tree)
	(improve-list (switch-consequents tree) dead?))
  (setf (switch-default tree) (improve (switch-default tree) dead?))
  tree)

(defun improve-scope-control-transfer (tree)
  (setf (scope-control-transfer-send-value tree)
	(improve (scope-control-transfer-send-value tree)))
  (setf (scope-control-transfer-destination-point tree)
	(improve (scope-control-transfer-destination-point tree)))
  tree)

(defun improve-control-point (tree)
  (typecase tree
    (dynamic-scope-control-point
     (setf (dynamic-scope-control-point-tag-name tree)
	   (improve (dynamic-scope-control-point-tag-name tree))))
    (dynamic-tag-control-point
     (setf (dynamic-tag-control-point-tag-name tree)
	   (improve (dynamic-tag-control-point-tag-name tree)))))
  tree)

(defun improve-unwind-protect (tree dead?)
  (setf (unwind-protect-protected-form tree)
	(improve (unwind-protect-protected-form tree)))
  (setf (unwind-protect-cleanup-form tree)
	(improve (unwind-protect-cleanup-form tree) dead?))
  tree)

(defun tail-leaves (tree)
  (unless (null tree)
    (typecase tree
      (seq  (let ((body (seq-body tree)))
	      (if (atom body)
		  (tail-leaves body)
		  (tail-leaves (car (last body))))))
      (scope-control-transfer 
       (tail-leaves (scope-control-transfer-send-value tree)))
      (unwind-protect 
	   (tail-leaves (unwind-protect-protected-form tree)))
      (if (append (tail-leaves (if-then tree))
		  (tail-leaves (if-else tree))))
      (switch (append (loop for c in (switch-consequents tree)
			    appending (tail-leaves c))
		      (tail-leaves (switch-default tree))))
      ((or function-call var-ref constant var-def mvalues) (list tree))
      (t nil))))

(defun clear-tree-leaves-tail-slots (tree)
  (unless (null tree)
    (setf (code-tail? tree) nil)
    (typecase tree
      (seq  (let ((body (seq-body tree)))
	      (if (atom body)
		  (clear-tree-leaves-tail-slots body)
		  (clear-tree-leaves-tail-slots (car (last body))))))
      (scope-control-transfer 
       (clear-tree-leaves-tail-slots (scope-control-transfer-send-value tree)))
      (unwind-protect 
	   (clear-tree-leaves-tail-slots (unwind-protect-protected-form tree)))
      (if (clear-tree-leaves-tail-slots (if-then tree))
	  (clear-tree-leaves-tail-slots (if-else tree)))
      (switch (loop for c in (switch-consequents tree)
		    do (clear-tree-leaves-tail-slots c))
	(clear-tree-leaves-tail-slots (switch-default tree)))
      ((or function-call var-ref constant var-def mvalues) (list tree))
      (t nil))))

;;; T or a list of side-effect classes (:CONS, :IO, :STORE)
(defun function-call-side-effects? (tree)
  tree
  t)
