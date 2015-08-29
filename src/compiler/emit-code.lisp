;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; Generate code from an analyzed code tree.
;;; 3 types of rvar: a var-name string, :LIVE or :DEAD
(defun emit-code (tree &optional (rvar :live) (rtype t))
  (unless (null tree)
    (etypecase tree
      (var-ref (emit-var-ref tree rvar rtype))
      (var-def (emit-var-def tree rvar rtype))
      (constant (emit-constant tree rvar rtype))
      (function-call (emit-function-call tree rvar rtype))
      (tag-control-point (emit-tag-control-point tree))
      (control-transfer (emit-control-transfer tree))
      (branch (emit-branch tree rvar rtype))
      (unwind-protect (emit-unwind-protect tree rvar rtype))
      (mvalues (emit-values tree rvar rtype))
      (seq (emit-seq tree rvar rtype))
      (c-code (emit-c-code tree rvar rtype)))))

(defun emit-c-code (tree rvar rtype)
  (let ((result-var (result-var-name rvar)))
    (maybe-emit-assignment result-var)
    (emit-c (c-code-string tree))
    (emit-c "~%")
    (emit-return-if-tail result-var tree rvar rtype)))

(defun result-var-name (rvar)
  (if (stringp rvar)
      rvar
      (ecase rvar
	(:live (tmp-var-name))
	(:dead rvar))))

(defun maybe-emit-assignment (rvar)
  (unless (eq rvar :dead) (emit-c "~A = " rvar)))

(defun emit-return-if-tail (value tree rvar rtype)
  (declare (ignore rtype))
  (if (code-tail? tree)
      (emit-lc tree "return(~A);~%" value)
      (if (stringp rvar)
	  (progn (unless (eq rvar value)
		   (emit-source-line tree)
		   (emit-c "~A = ~A;~%" rvar value))
		 rvar)
	  value)))

(defun emit-constant (tree rvar rtype)
  (emit-return-if-tail (emit-lref (constant-data tree))
		       tree
		       rvar
		       rtype))

(defun emit-var-ref (tree rvar rtype)
  (emit-return-if-tail
   (let ((var (var-ref-var tree)))
     (ecase (var-extent var)
       (:dynamic (var-c-name var))
       (:indefinite (format nil "GET_OE_SLOT(~A,~D)"
			    (proc-oe-var
			     (var-ref-innermost-proc tree))
			    (lookup-outer-environment-offset var)))))
   tree
   rvar
   rtype))

(defun emit-var-def (tree rvar rtype)
  (let* ((var (var-def-var tree))
	 (value
	  (ecase (var-extent var)
	    (:dynamic (emit-code (var-def-value tree)
				 (var-c-name var)
				 t))
	    (:indefinite
	     (let ((value (emit-code (var-def-value tree) :live t)))
	       (emit-c  "SET_OE_SLOT(~A,~D,~A);~%"
			(proc-oe-var
			 (var-def-innermost-proc tree))
			(lookup-outer-environment-offset var)
			value)
	       value)))))
    (emit-return-if-tail value tree rvar rtype)))

(defun emit-function-call (tree rvar rtype)
  (if (inline-array-op? tree)
      (emit-inline-array-op tree rvar rtype)
      (etypecase tree
	(primitive-call (emit-primitive-call tree rvar rtype))
	(foreign-call (emit-foreign-call tree rvar rtype))
	(named-call (emit-named-call tree rvar rtype))
	(c-struct-op (emit-c-struct-op tree rvar rtype))
	(unnamed-call (emit-unnamed-call tree rvar rtype)))))

(defun emit-seq (tree rvar rtype)
  (typecase tree
    (proc (emit-proc tree rvar rtype))
    (named-local (emit-named-local tree rvar rtype))
    (inline-mv-call (emit-inline-mv-call tree rvar rtype))
    (scope-seq (emit-scope-seq tree rvar rtype))
    (tag-seq (emit-tag-seq tree rvar rtype))
    (spec-bind-seq (emit-spec-bind-seq tree rvar rtype))
    (t (emit-body (seq-body tree) rvar rtype))))

(defun code-mv? (code)
  (not (null (code-mv-holder code))))

(defun emit-proc (tree rvar rtype)
  (emit-proc-extern-declaration tree)
  (if *emitting-proc?*
      (progn (push tree *inner-procs*)
	     (emit-return-if-tail
	      (if (inner-proc-needs-oe? tree)
		  (emit-cons-closure tree (proc-c-name tree))
		  (emit-lref tree))
	      tree
	      rvar
	      rtype))
      (emit-proc-code tree)))

(defun emit-proc-code (tree)
  (let ((*inner-procs* nil))
    (let ((*emitting-proc?* t))
      (emit-source-line tree)
      (emit-ansi-proc-prototype tree *c-stream*)
      (emit-c "~%{~%")
      (let ((*tmp-var-counter* -1))
	(loop for v being the elements of  (proc-vars-to-declare tree)
	      using (index i) do
	      (progn (emit-c "~ALP ~A; "
			     (proc-volatile tree)
			     (var-c-name v))
		     (when (= 2 (mod i 3))
		       (emit-c "~%")))
	      finally (emit-c "~%"))
	(loop for i from 0 to (proc-max-tmp-var-count tree) do
	      (progn (emit-c "~ALP t~A; " (proc-volatile tree) i) 
		     (when (= 5 (mod i 6))
		       (emit-c "~%")))
	      finally (emit-c "~%"))
	(let ((var-info (proc-var-info tree)))
	  (if (var-info-hairy? var-info)
	      (emit-hairy-arg-code tree)
	      (emit-required-argc-check tree))
	  (unless (null (proc-start-label tree))
	    (emit-c "~A:~%" (proc-start-label tree)))
	  (emit-binder-body tree (var-info-all-vars var-info) :live t))
	(emit-c "}~%~%")))
    (loop for p in *inner-procs* do (emit-proc-code p))
    (proc-c-name tree)))
    
(defun emit-inline-mv-call (tree rvar rtype)
  (let* ((var-info (inline-mv-call-var-info tree))
	 (all-vars (var-info-all-vars var-info))
	 (mv-holder (inline-mv-call-new-holder tree)))
    (emit-c "{~%")
    (emit-misc-storage-decls var-info)
    (emit-c "BEGIN_MV_CALL(~A,~D);~%" mv-holder 0)
    (let ((value-var (emit-code
		      (first (inline-mv-call-values tree)) :live t)))
      (unless (null all-vars)
	(emit-c "SET_MV_RETURN_VALUE(~A,0,~A);~%" mv-holder value-var)))
    ;; HEY! use a separate vcnt and skip this step
    (emit-c "if SV_RETURN_P(~A) SET_MV_RETURN_COUNT(~A,1);~%"
	    mv-holder mv-holder)
    (emit-hairy-value-code var-info mv-holder)
    (emit-c "END_MV_CALL;~%")
    (let ((body-value (emit-binder-body tree all-vars rvar rtype)))
      (emit-c "}~%")
      body-value)))

(defun emit-values (tree rvar rtype)
  (let ((args-lhs (loop for arg in (mvalues-args tree)
			collect (emit-code arg :live t)))
	(holder-name (code-mv-holder tree)))
    (when (code-mv? tree)
      (flet ((emit-fill-holder ()
	       (emit-c "SET_MV_RETURN_FLAG(~A);~%" holder-name)
	       (emit-c "SET_MV_RETURN_COUNT(~A,~D);~%"
		       holder-name (length args-lhs))
	       (loop for lhs being the elements of  (cdr args-lhs)
		     using (index i)
		     do (emit-c "SET_MV_RETURN_VALUE(~A,~D,~A);~%"
				holder-name (+ i 1) lhs))))
	(if (string= holder-name argc-var-name)	; constant folding screws eq!
	    (progn (emit-c "if (MV_HOLDER_P(~A)) {~%" holder-name)
		   (emit-fill-holder)
		   (emit-c "}~%"))
	    (emit-fill-holder))))
    (emit-return-if-tail (if (null args-lhs) "NIL" (car args-lhs))
			 tree
			 rvar
			 rtype)))

(defun inner-proc-needs-oe? (tree)
  (or (not (null (inner-proc-oe-refs tree)))
      (inner-proc-pass-on-oe? tree)))

(defun setup-oe-if-needed (tree)
  (etypecase tree
    (top-level-proc
     (unless (null (top-level-proc-oe-vars tree))
       (let ((oe-var (tmp-var-name)))       
	 (setf (proc-oe-var tree) oe-var)
	 (emit-c "~A = NEW_OE(~D);~%"
		 oe-var (length (top-level-proc-oe-vars tree))))))
    (inner-proc
     (when (inner-proc-needs-oe? tree)
       (let ((oe-var (tmp-var-name)))
	 (setf (proc-oe-var tree) oe-var)
	 (emit-c  "~A = OE;~%" oe-var))))))

(defun emit-cons-closure (tree label)
  (let ((result-var (tmp-var-name)))
    (emit-c "~A = MAKE_CLOSURE(~A,~A);~%"
	    result-var
	    label
	    (proc-oe-var (first (inner-proc-parent-chain tree))))
    result-var))

(defun emit-spec-bind-seq (tree rvar rtype)
  (let* ((specs (spec-bind-seq-specials tree))
	 (values (loop for value in (spec-bind-seq-values tree)
		       collect (emit-code value :live t)))
	 (symbol-names (mapcar #'(lambda (v)
				   (let ((sym (var-name v)))
				     (emit-data sym)
				     (lisp->c-symbol-name sym)))
			       specs)))
    (loop for val in values
	  for name in symbol-names do
	  (emit-c "BEGIN_SPEC_BIND(~A,~A);~%" name val))
    (let ((value (emit-body (seq-body tree) :live t)))
      (loop for name in (nreverse symbol-names)
	    do (emit-c "END_SPEC_BIND(~A);~%" name))
      (emit-return-if-tail value tree rvar rtype))))

(defun emit-body (list rvar rtype)
  (if (atom list)
      (emit-code list :live t)
      (loop with end = (last list)
	    for x on list
	    for var = (if (eq x end)
			  (emit-code (car x) rvar rtype)
			  (emit-code (car x) :dead t))
	    finally (return var))))

(defun emit-binder-body (tree all-vars rvar rtype)
  (loop for var in all-vars
	when (eq (var-extent var) :indefinite) 
	do (emit-c "SET_OE_SLOT(~A,~D,~A);~%"
		   (proc-oe-var (var-innermost-proc var))
		   (lookup-outer-environment-offset var)
		   (var-c-name var)))
  (emit-code (seq-body tree) rvar rtype))

(defun emit-named-local (tree rvar rtype)
  (let ((all-vars (named-local-vars tree)))
    (loop for value in (named-local-values tree)
	  for var in all-vars
	  collect (emit-code value (var-c-name var) t))
    (emit-binder-body tree all-vars rvar rtype)))

(defun add-external-proc (name)
  (let ((c-name (lisp->c-proc-name name)))
    (unless (eq (gethash name *external-procs*) :done)
      (setf (gethash name *external-procs*) c-name))
    c-name))

(defun emit-named-call (tree rvar rtype)
  (let* ((result-var (result-var-name rvar))
	 (arg-lvalues (mapcar #'(lambda (arg)
				  (emit-code arg :live t))
			      (function-call-args tree)))
	 (argc (length arg-lvalues))
	 (name (function-call-name tree)))
    (if (named-call-emit-as-goto? tree)
	(let ((proc (named-call-emit-as-goto? tree)))
	  (loop for var in (var-info-requireds
			    (proc-var-info proc))
		for arg-lvalue in arg-lvalues
		unless (eq (var-c-name var) arg-lvalue)	; skip useless assign
		do (emit-c "~A = ~A; " (var-c-name var) arg-lvalue))
	  (emit-source-line tree)
	  (emit-c "~%goto ~A;~%" (proc-start-label proc)))
	(progn
	  (emit-source-line tree)
	  (maybe-emit-assignment result-var)
	  (if (config-indirect-calls? *config*)
	      (emit-c "ICALL(~A) (" (emit-data name))
	      (let ((c-name (add-external-proc name)))
		(emit-c "(LP) ~A(" c-name)))
	  (if (code-mv? tree)
	      (emit-c "MV_CALL(~A,~D)" (code-mv-holder tree) argc)
	      (emit-c "~D" argc))
	  (loop for arg-lvalue in arg-lvalues
		do (emit-c ", ~A" arg-lvalue))
	  (emit-c ");~%")
	  (emit-return-if-tail result-var tree rvar rtype)))))

(defun emit-unnamed-call (tree rvar rtype)
  (let* ((result-var (result-var-name rvar))
	 (arg-lvalues (mapcar #'(lambda (arg)
				  (emit-code arg :live t))
			      (function-call-args tree)))
	 (func-var (emit-code (unnamed-call-function-form tree) :live t))
	 (func (case (code-out-type (unnamed-call-function-form tree))
		 (compiled-function func-var)
		 (symbol (format nil "LDREF(~A,SYMBOL,function)" func-var))
		 (t (format nil "COERCE_TO_FUNCTION(~A)" func-var))))
	 (argc (+ (if (unnamed-call-spread-args? tree) 1 0)
		  (length arg-lvalues))))
    (emit-source-line tree)
    (maybe-emit-assignment result-var)
    (if (unnamed-call-spread-args? tree)
	(emit-c "p_lsp_APPLY(")
	(emit-c "CODE_PTR(~A)(" func))
    (if (code-mv? tree)
	(emit-c "MV_CALL(~A,~D)" (code-mv-holder tree) argc)
	(emit-c "~D" argc))
    (when (unnamed-call-spread-args? tree)
      (emit-c ", ~A" func))
    (loop for arg-lvalue in arg-lvalues do
	  (emit-c ", ~A" arg-lvalue))
    (emit-c ");~%")
    (emit-return-if-tail result-var tree rvar rtype)))
  
(defun emit-primitive-call (tree rvar rtype)
  (let* ((finfo (function-call-info tree))
	 (emitter (primitive-info-emitter finfo))
	 (result-var (result-var-name rvar))
	 (arg-lvalues
	  (mapcar #'(lambda (arg type)
		      (convert-lisp->c type (emit-code arg :live t) arg))
		  (function-call-args tree)
		  (function-info-in-types finfo)))
	 (out-type (car (primitive-info-out-types finfo)))
	 (out-converter (c->lisp-converter out-type)))
    (emit-source-line tree)
    (if (eq out-type c-type-if-test)
	(if (eq (code-out-type tree) c-type-if-test)
	    (progn (emit-c "if (")
		   (apply emitter arg-lvalues))
	    (progn (maybe-emit-assignment result-var)
		   (emit-c "(")
		   (apply emitter arg-lvalues)
		   (emit-c " ? T : NIL);~%")))
	(if (eq out-type (code-out-type tree))
	    (apply emitter arg-lvalues)
	    (progn (unless (null (primitive-info-outs finfo))
		     (maybe-emit-assignment result-var))
		   (emit-c out-converter)
		   (apply emitter arg-lvalues)
		   (emit-c ");~%"))))
    (emit-return-if-tail result-var tree rvar rtype)))

(defun emit-foreign-call (tree rvar rtype)
  (let* ((foreign-info (function-call-info tree))
	 (result-var (result-var-name rvar))
	 (arg-lvalues (mapcar #'(lambda (arg)
				  (emit-code arg :live t))
			      (function-call-args tree)))
	 (out-type (first (foreign-info-out-type-objects foreign-info)))
	 (out-converter (c->lisp-converter out-type)))
    (record-foreign-function-reference foreign-info)
    (emit-source-line tree)
    (unless (eq out-type c-type-void)
      (maybe-emit-assignment result-var))
    (emit-c out-converter)
    (emit-c "~A(" (foreign-info-foreign-name foreign-info))
    (iterate emit-args ((argls arg-lvalues)
			(args (function-call-args tree))
			(types (foreign-info-in-type-objects foreign-info)))
      (if (null argls)
	  (emit-c "));~%")
	  (let ((arg-lvalue (car argls))
		(arg (car args))
		(type (car types)))
	    (emit-c (convert-lisp->c type arg-lvalue arg))
	    (unless (null (cdr argls)) (emit-c ", "))
	    (emit-args  (cdr argls)
			(cdr args)
			(cdr types)))))
    (emit-return-if-tail (if (eq out-type c-type-void)
			     "NIL"
			     result-var)
			 tree rvar rtype)))

(defun code-with-side-effects? (tree)
  (not (typep tree 'constant)))

(defun emit-branch (tree rvar rtype)
  (etypecase tree
    (if (emit-if tree rvar rtype))
    (switch (emit-switch tree rvar rtype))))

(defun emit-if (tree rvar rtype)
  (emit-source-line tree)
  (if (null (if-then tree))
      (if (null (if-else tree))
	  (error "Why didn't improve zap this IF?")
	  (emit-partial-if tree rvar rtype #'if-else t))
      (if (null (if-else tree))
	  (emit-partial-if tree rvar rtype #'if-then nil)
	  (emit-full-if tree rvar rtype))))

(defun emit-switch (tree rvar rtype)
  (let* ((switch-var (result-var-name rvar))
	 (test-lvalue (emit-code (switch-test tree) :live t)))
    (emit-source-line tree)
    (emit-c "switch ((long) ~A) {~%" test-lvalue)
    (loop for key in (switch-keys tree)
	  for consequent in (switch-consequents tree)
	  do (progn (dolist (k key)
		      ;; HEY! Fix this when we can have raw data selector
		      (emit-c "case ~D:~%" (ash k 1))) ; int -> fixnum
		    (emit-code consequent switch-var rtype)
		    (emit-c "break;~%")))
    (emit-c "default:~%")
    (emit-code (switch-default tree) switch-var rtype)
    (emit-c "break;~%}~%")
    switch-var))

(defun emit-full-if (tree rvar rtype)
  (let* ((if-var (result-var-name rvar))
	 (test-lvalue (emit-code (if-test tree) :live t)))
    (if (branch-inline-test? tree)
	(emit-c ") {~%")
	(emit-c "if (~A != NIL) {~%" test-lvalue))
    (emit-code (if-then tree) if-var rtype)
    (emit-lc (if-else tree) "} else {~%")
    (emit-code (if-else tree) if-var rtype)
    (emit-c "}~%")
    if-var))

(defun emit-partial-if (tree rvar rtype branch-selector invert-test?)
  (let ((if-var (result-var-name rvar))
	(test-lvalue (emit-code (if-test tree) :live t)))
    (if (branch-inline-test? tree)
	(if invert-test?
	    (emit-c " == 0) {~%")
	    (emit-c ") {~%"))
	(if invert-test?
	    (emit-c "if (~A == NIL) {~%" test-lvalue)
	    (emit-c "if (~A != NIL) {~%" test-lvalue)))
    (emit-code (funcall branch-selector tree) if-var rtype)
    (emit-c "}~%")
    if-var))

(defun emit-scope-seq (tree rvar rtype)
  (let* ((cp (scope-seq-control-point tree))
	 (receive-var (result-var-name rvar)))
    (etypecase cp
      (dynamic-scope-control-point
       (let ((tag-obj (emit-code (dynamic-scope-control-point-tag-name cp)
				 :live
				 t))
	     (mv-holder (code-mv-holder tree)))
	 (emit-c "BEGIN_CATCH(~A,~A);~%"
		 tag-obj (if (null mv-holder) 0 mv-holder))
	 (let ((seq-var (emit-body (seq-body tree) rvar rtype)))
	   (unless (or (eq receive-var :dead)
		       (eq receive-var seq-var))
	     (emit-c "~A = ~A;~%" receive-var seq-var)))
	 (emit-c "END_CATCH(~A);~%" (if (eq receive-var :dead)
					(tmp-var-name)
					receive-var))
	 (emit-return-if-tail receive-var tree rvar rtype)))
      (static-scope-control-point
       (setf (scope-control-point-receive-var cp) receive-var)
       (let ((seq-var (emit-body (seq-body tree) rvar rtype)))
	 (if (code-tail? tree)
	     nil
	     (progn (unless (eq receive-var :dead)
		      (emit-c "~A = ~A;~%" receive-var seq-var))
		    (emit-c "~A:;~%" (static-scope-control-point-c-name cp))
		    receive-var)))))))

(defun emit-tag-seq (tree rvar rtype)
  (loop for cp in (tag-seq-control-points tree)
	when (dynamic-tag-control-point-p cp)
	count cp into dynamic-tag-count
	and
	do (let ((tag-obj (emit-code (dynamic-tag-control-point-tag-name cp)
				     :live
				     t)))
	     (emit-c "BEGIN_DYNAMIC_TAG(~A,~A)~%"
		     tag-obj
		     (tag-control-point-c-name cp)))
	finally (progn (emit-body (seq-body tree) rvar rtype)
		       (dotimes (i dynamic-tag-count)
			 (emit-c "END_DYNAMIC_TAG~%"))))
  (emit-return-if-tail "NIL" tree rvar rtype))
		   
(defun emit-control-transfer (tree)
  (etypecase tree
    (scope-control-transfer (emit-scope-control-transfer tree))
    (tag-control-transfer (emit-tag-control-transfer tree))))

(defun emit-scope-control-transfer (tree)
  (let ((point (control-transfer-destination-point tree))
	(value-tree (scope-control-transfer-send-value tree)))
    (etypecase point
	(static-scope-control-point
	 (let ((send-var (emit-code value-tree :live t))
	       (scope (scope-control-point-parent point)))
	   (if (code-tail? scope)
	       (emit-return-if-tail send-var scope :live t)
	       (let ((rvar (scope-control-point-receive-var point)))
		 (unless (eq rvar :dead)
		   (emit-c "~A = ~A;~%" rvar send-var))
		 (emit-c "goto ~A;~%"
			 (static-scope-control-point-c-name point))))))
      (dynamic-scope-control-point
       (let ((tag-obj
	      (emit-code (dynamic-scope-control-point-tag-name point) :live t))
	     (throw-mv-holder
	      (scope-control-point-receive-var point)))
	 (when (eq throw-mv-holder :dead)
	   (error "fix me"))
	 (emit-c "BEGIN_MV_CALL(~A,~D);~%" throw-mv-holder 0)
	 (let ((send-var (emit-code value-tree :live t)))
	   (emit-c "THROW(~A,~A,~A);~%"
		   tag-obj send-var throw-mv-holder))
	 (emit-c "END_MV_CALL;~%"))))))

(defun emit-tag-control-transfer (tree)
  (let ((point (control-transfer-destination-point tree)))
    (etypecase point
      (static-tag-control-point
       (emit-c "goto ~A;~%" (static-tag-control-point-c-name point)))
      (dynamic-tag-control-point
       (let ((tag-obj (emit-code (dynamic-tag-control-point-tag-name point)
				 :live
				 t)))
	 (emit-c "GOTO_DYNAMIC_TAG(~A);~%" tag-obj))))))

		   
(defun emit-tag-control-point (tree)
  (emit-c "~A:;~%" (tag-control-point-c-name tree)))

(defun emit-unwind-protect (tree rvar rtype)
  (emit-c "BEGIN_UW_PROTECT_BODY~%")
  (let ((value (emit-code (unwind-protect-protected-form tree) :live t)))
    (emit-c "BEGIN_UW_PROTECT_CLEANUP~%")
    (emit-code (unwind-protect-cleanup-form tree) :live t)
    (emit-c "CONTINUE_FROM_PROTECT~%")
    (emit-return-if-tail value tree rvar rtype)))

(defun lookup-outer-environment-offset (var)
  (let* ((inner (var-innermost-proc var))
	 (outer (if (top-level-proc-p inner)
		    inner
		    (first (last (inner-proc-parent-chain inner))))))
    (position var (top-level-proc-oe-vars outer))))

(defun genstring (name)
  (concatenate 'string name (write-to-string (incf *string-counter*))))
  
(defun tmp-var-name ()
  (concatenate 'string "t" (write-to-string (incf *tmp-var-counter*))))

(defun emit-proc-extern-declaration (tree)
  (let* ((info (proc-var-info tree))
	 (hairy? (var-info-hairy? info)))
    (emit-k "extern ")
    (emit-ansi-proc-prototype tree *k-stream*)
    (emit-k ";~%")
    (setf (gethash (proc-name tree) *external-procs*)
	  :done)))

(defun emit-ansi-proc-prototype (tree emitter-stream)
  (let ((info (proc-var-info tree)))
    (format emitter-stream "LP ~A(ARGC argc" (proc-c-name tree))
    (loop for var in (var-info-requireds info)
	  do (format emitter-stream  ", ~ALP ~A"
		     (proc-volatile tree) (var-c-name var)))
    (if (var-info-hairy? info)
	(format emitter-stream  ",...)")
	(format emitter-stream ")"))))

(defun emit-required-argc-check (tree)
  (when (config-argc-check? *config*)
    (let ((len (length (var-info-requireds (proc-var-info tree)))))
      (emit-c "if (argc != ~D) wna(argc,~D);~%" len len)))
  (setup-oe-if-needed tree))

(defun emit-misc-storage-decls (info)
  (unless (null (var-info-restv-var info))
    (emit-c "RESTV_HOLDER(restv_vector);~%"))
  (let ((rest-var (var-info-rest-var info)))
    (when (and (not (null rest-var)) (var-dynamic-extent? rest-var))
      (emit-c "DYNAMIC_REST_HOLDER(rest_conses);~%"))
    (emit-c "long real_argc;~%")))

(defun emit-hairy-arg-code (tree)
  (let* ((info (proc-var-info tree))
	 (reqs (var-info-requireds info))
	 (num-requireds (length reqs)))
    (emit-misc-storage-decls info)
    (emit-c "BEGIN_ANSI_VAR_ARGS(~A);~%"
	    (if (null reqs)
		"argc"
	      (var-c-name (first (last reqs)))))
    (setup-oe-if-needed tree)
    (emit-c "real_argc = REAL_ARGC(argc);~%")
    (when (config-argc-check? *config*)
      (emit-c "if (real_argc < ~D) wna_low(real_argc,~D);~%"
	      num-requireds num-requireds)
      (when (and (null (var-info-restv-var info))
		 (null (var-info-rest-var info)))
	(let ((max-argc (+ num-requireds
			   (length (var-info-optionals info)))))
	  (emit-c "if (real_argc > ~D) wna_high(real_argc,~D);~%"
		  max-argc max-argc))))
    (emit-hairy-var-code info
			 "NEXT_VAR_ARG"
			 "END_VAR_ARGS")))

(defun emit-hairy-value-code (info mv-holder)
  (emit-c "real_argc = GET_MV_RETURN_COUNT(~A);~%" mv-holder)
  (emit-c "BEGIN_VAR_VALUES;~%")
  (emit-hairy-var-code info
		      (format nil "NEXT_VAR_VALUE(~A)" mv-holder)
		      "END_VAR_VALUES"))

(defun emit-hairy-var-code (info next-var end-var)
  (let ((num-requireds (length (var-info-requireds info))))
    (emit-optional-arg-code (+ num-requireds 1)
			    (var-info-optionals info)
			    next-var)
    (emit-rest-arg-code info next-var)
    (emit-restv-arg-code info next-var)
    (emit-keyword-arg-code (var-info-keys info)
			   (var-info-rest-var info))
    (emit-c "~A;~%" end-var)))
  
(defun emit-optional-arg-code (start opts next-var-arg)
  (loop for opt being the elements of opts using (index i) do
	(progn
	  (emit-c "if (real_argc < ~D) {~%" (+ start i))
	  (let ((lvalue (emit-code (optional-init-form opt) :live t)))
	    (emit-c "~A = ~A;~%"
		    (var-c-name (optional-var opt))
		    lvalue))
	  (unless (null (optional-supplied-var opt))
	    (emit-c "~A = NIL;~%"
		    (var-c-name (optional-supplied-var opt))))
	  (emit-c "} else {~%")
	  (emit-c "~A = ~A;~%" (var-c-name (optional-var opt)) next-var-arg)
	  (unless (null (optional-supplied-var opt))
	    (emit-c "~A = T;~%"
		    (var-c-name (optional-supplied-var opt))))
	  (emit-c "}~%"))))

(defun emit-rest-arg-code (info next-var-arg)
  (unless (null (var-info-rest-var info))
    (emit-c "~A(~A,~D,~A);~%"
	    (if (var-dynamic-extent? (var-info-rest-var info))
		"DYNAMIC_RESTIFY"
		"RESTIFY")
	    (var-c-name (var-info-rest-var info))
  	    (+ (length (var-info-requireds info))
	       (length (var-info-optionals info))
	       1)
	    next-var-arg)))

(defun emit-restv-arg-code (info next-var-arg)
  (unless (null (var-info-restv-var info)) 
    (emit-c "RESTVIFY(~A,~D,~A);~%"
	    (var-c-name (var-info-restv-var info))
  	    (+ (length (var-info-requireds info))
	       (length (var-info-optionals info))
	       1)
	    next-var-arg)))

(defun emit-keyword-arg-code (keys rest-var)
  (loop for key in keys do
	(progn
	  (unless (null (key-supplied-var key))
	    (emit-c "~A = T;~%" (var-c-name (key-supplied-var key))))
	  ;; HEY! pass allow other keys along...
	  (emit-c "BEGIN_KEY_INIT(~A,~A,~A)~%"
		  (var-c-name (key-var key))
		  (emit-code (key-name key) :live t)
		  (var-c-name rest-var))
	  (unless (null (key-supplied-var key))
	    (emit-c "~A = NIL;~%"   (var-c-name (key-supplied-var key))))
	  (let ((init-lvalue (emit-code (key-init-form key) :live t)))
	    (emit-c "~A = ~A;~%" (var-c-name (key-var key)) init-lvalue))
	  (emit-c "END_KEY_INIT~%"))))
 

;;; Emit direct array refs for simple-vectors of known type
;;; and for simple-arrays of known type and all but 1 dim
;;; 1d: x = ((double (*)) ptr)[1];
;;; 2d: x = ((double (*)[3]) ptr)[1][2];
(defun inline-array-op? (call)
  (let ((name (function-call-name call)))
    (and (typep call '(or named-call foreign-call))
	 (member name '(aref vref set-aref set_vref))
	 (let* ((array-arg (if (member name '(aref vref))
			       (first (function-call-args call))
			       (second (function-call-args call))))
		(array-type (code-out-type array-arg)))
	   (and (listp array-type)
		(eq (car array-type) 'simple-array)
		;; HEY! fix this so that bit ops are inlined also
		(not (equal (second array-type) '(integer 0 1)))
		(let ((dims (third array-type)))
		  ;; Don't need to know first dimension
		  (every #'numberp (cdr dims))))))))

(defun emit-inline-array-op (call rvar rtype)
  (ecase (function-call-name call)
    ((aref vref) (emit-inline-array-ref call rvar rtype))
    ((set-aref set_vref) (emit-inline-array-def call rvar rtype))))

(defun emit-inline-array-ref (call rvar rtype)
  (let* ((array-arg (first (function-call-args call)))
	 (index-args (rest (function-call-args call)))
	 (array-type (code-out-type array-arg))
	 (element-type (second array-type))
	 (declared-dims (third array-type))
	 (c-type-object (lisp-array-type->c-type-object element-type))
	 (result-var (result-var-name rvar))
	 (array (emit-code array-arg :live t))
	 (indices (loop for index in index-args
			collect (emit-code index :live t)))
	 (real-array (if (> (length indices) 1)
			 (format nil "((LP) DEREF(~A))" array)
			 array)))
    (unless (eq result-var :dead)
      (emit-c
       "~A = (LP) ~A(((~A (*)~{[~A]~}) (~A - 1))~{[FX_TO_LONG(~A)]~});~%"
       result-var
       (c-type-info-convert-to-lisp c-type-object)
       (c-type-info-c-type c-type-object)
       (cdr declared-dims)
       real-array
       indices)
      (emit-return-if-tail result-var call :live rtype))))

(defun emit-inline-array-def (call rvar rtype)
  (let* ((value-arg (first (function-call-args call)))
	 (array-arg (second (function-call-args call)))
	 (index-args (cddr (function-call-args call)))
	 (array-type (code-out-type array-arg))
	 (element-type (second array-type))
	 (declared-dims (third array-type))
	 (c-type-object (lisp-array-type->c-type-object element-type))
	 (value (emit-code value-arg rvar rtype))
	 (array (emit-code array-arg :live t))
	 (indices (loop for index in index-args
			collect (emit-code index :live t)))
	 (real-array (if (> (length indices) 1)
			 (format nil "((LP) DEREF(~A))" array)
			 array)))
    (emit-c
     "((~A (*)~{[~A]~}) (~A - 1))~{[FX_TO_LONG(~A)]~} = (~A)~A(~A);~%"
     (c-type-info-c-type c-type-object)
     (cdr declared-dims)
     real-array
     indices
     (c-type-info-c-type c-type-object)
     (c-type-info-convert-to-c c-type-object)
     value)
    (emit-return-if-tail value call :live t)))

;;; This is sort of gross. 
(defun lisp-array-type->c-type-object (type)
  (select (type->element-type-tag type)
    (element-type-bit (error "HEY! fix this.") c-type-int32)
    (element-type-signed-8bit c-type-int8)
    (element-type-unsigned-8bit c-type-uint8)
    (element-type-signed-16bit c-type-int16)
    (element-type-unsigned-16bit c-type-uint16)
    (element-type-signed-32bit c-type-int32)
    (element-type-unsigned-32bit c-type-uint32)
    (element-type-ptr c-type-lptr)
    (element-type-float c-type-double)
    (element-type-char c-type-char)))

(defun emit-source-line (tree)
  (when (config-lisp-line-numbers? *config*)
    (let* ((line (code-line tree)))
      (emit-c "# ~D~%" line)
      (format t "Line number: ~D, tree: ~A~%" line tree))))


;; s = NEW_FPTR(stat(),s_w_STAT);
;; x = LONG_TO_FX((struct stat*) (LDREF(FPTR,s,data)))->st_spare1);
;; y = MAKE_FPTR((struct stat*) (LDREF(FPTR,s,data)))->st_time-t, s_w_TIME_T);
;; z = LONG_TO_FX((((struct stat*) (LDREF(FPTR,s,data)))->stat-st-spare4);
;; ((struct stat*) (LDREF(FPTR,s,data)))->st_spare1) = FX_TO_LONG(z);

(defun emit-c-struct-op (tree rvar rtype)
  (etypecase tree
    (c-struct-ref (emit-c-struct-ref tree rvar rtype))
    (c-struct-def (emit-c-struct-def tree rvar rtype))))

(defun emit-c-struct-ref (tree rvar rtype)
  (let* ((info (c-struct-op-struct-info tree))
	 (struct-name (c-struct-info-name info))
	 (field (c-struct-op-field tree))
	 (struct (emit-code (first (function-call-args tree)) :live t))
	 (result-var (result-var-name rvar)))
    (record-c-info-reference info)
    (unless (eq result-var :dead)
      (emit-c
       "~A = ~A(((struct ~A *) (RAW_FPTR(~A)))->~A));~%"
       result-var
       (c->lisp-converter
	(c-type-name->c-type-object (c-struct-slot-type field)))
       struct-name
       struct
       (c-struct-slot-c-name field))
      (emit-return-if-tail result-var tree :live rtype))))

(defun emit-c-struct-def (tree rvar rtype)
  (let* ((info (c-struct-op-struct-info tree))
	 (struct-name (c-struct-info-name info))
	 (field (c-struct-op-field tree))
	 (struct (emit-code (first (function-call-args tree)) rvar rtype))
	 (value (emit-code (second (function-call-args tree)) :live t))
	 (result-var (result-var-name rvar)))
    (record-c-info-reference info)
    (unless (eq result-var :dead)
      (emit-c
       "ref = ~A;~%"
       (c->lisp-converter
	(c-type-name->c-type-object (c-struct-slot-type field)))
       struct-name
       struct
       (c-struct-slot-c-name field)
       value))
    (emit-return-if-tail value tree :live rtype)))

(defun record-c-info-reference (info)
  (pushnew info *referenced-c-info*))

(defun record-c-struct-reference (info)
  (record-c-info-reference info))

(defun record-foreign-function-reference (info)
  ;; HEY! also record c struct refs
  (record-c-info-reference info))

(defun emit-referenced-c-definitions ()
  (dolist (i *referenced-c-info*)
    (emit-c-definition i))
  (emit-k "~%~%"))

(defun emit-c-definition (info)
  (etypecase info
    (c-struct-info (emit-c-struct-definition info))
    (foreign-info (emit-foreign-definition info))))

(defun emit-c-struct-definition (info)
  (emit-k "struct ~A {~%" (c-struct-info-name info))
  (loop for slot in (c-struct-info-slots info)
	do (emit-k "~A;~%"
		   (c-type-decl
		    (c-type-name->c-type-object (c-struct-slot-type slot))
		    (c-struct-slot-c-name slot))))
  (emit-k "};~%"))

(defun emit-foreign-definition (info)
  (emit-k "extern ~A ~A("
	  (c-type-spec (car (foreign-info-out-type-objects info)))
	  (foreign-info-foreign-name info))
  (loop for out-type on (foreign-info-in-type-objects info)
	  do (emit-k "~A" (c-type-spec (car out-type)))
	  unless (null (cdr out-type))
	  do (emit-k ", "))
  (emit-k ");~%"))
				 
(defun convert-lisp->c (type arg tree)
  (if (and (constant-p tree)
	   (fixnump (constant-data tree))
	   (not (null (c-type-info-constant-to-c type))))
      (format nil "~D" (funcall (c-type-info-constant-to-c type)
				(constant-data tree)))
      (etypecase type
	(c-type-info (let ((converter (c-type-info-convert-to-c type)))
		       (if (null converter)
			   arg
			   (format nil "~A(~A)" converter arg))))
	(c-constant-type-info
	 (convert-lisp->c (c-constant-type-info-type type) arg tree))
	(c-array-info (typecase (c-array-info-element-type type)
			(c-struct-info (format nil "RAW_FPTR(~A)" arg))
			;; HEY! Fix this
			(t (format nil "lisp_to_c_array(~A)" arg))))
	(c-struct-info (error "struct case")))))
				 
(defun c->lisp-converter (type)
  (etypecase type
    (c-type-info (format nil "~A(" (c-type-info-convert-to-lisp type)))
    (c-constant-type-info (c->lisp-converter (c-constant-type-info-type type)))
    (c-array-info
     (let ((element-type (c-array-info-element-type type))
	   (dims (c-array-info-dimensions type)))
       (case (length dims)
	 (0 (typecase element-type
	      (c-struct-info
	       (let ((name (emit-lref (c-struct-info-name element-type))))
		 (format nil "NEW_FPTR(~A, " name)))
	      (t (select element-type
		   (c-type-char "c_to_lisp_string((LP) ")
		   (t (error "fix me!"))))))
	 (1 (typecase element-type
	      (c-struct-info (error "fix me"))
	      (t (select element-type
		   (c-type-char "c_to_lisp_string((LP) ")
		   (t (error "fix me!"))))))
	 (t (error "finish multi-array converters")))))
    (c-struct-info (error "write struct"))))

(defun c-type-spec (type)
  (etypecase type
    (c-type-info (c-type-info-c-type type))
    (c-constant-type-info
     (format nil "const ~A"
	     (c-type-spec (c-constant-type-info-type type))))
    (c-array-info
     (format nil "~A *" (c-type-spec (c-array-info-element-type type))))
    (c-struct-info (format nil "struct ~A" (c-struct-info-name type)))))

(defun c-type-decl (type var-name)
  (etypecase type
    (c-type-info (format nil "~A ~A" (c-type-info-c-type type) var-name))
    (c-constant-type-info (format nil "const ~A ~A"
				  (c-type-info-c-type
				   (c-constant-type-info-type type))
				  var-name))
    (c-array-info
     (let ((dims (c-array-info-dimensions type))
	   (type-spec (c-type-spec (c-array-info-element-type type))))
       (if (null dims)
	   (format nil "~A *~A" type-spec var-name)
	   (format nil "~A ~A~{[~D]~}" type-spec var-name dims))))
    (c-struct-info (format nil "struct ~A ~A"
			   (c-struct-info-name type) var-name))))

(defun c-type-name->c-type-object (type)
  (if (atom type)
      (case type
	(char c-type-char)
	(int8 c-type-int8)
	((unsigned-char unsigned-int8) c-type-uint8)
	((short int16) c-type-int16)
	((unsigned-short unsigned-int16) c-type-uint16)
	((int int31) c-type-int31)
	((unsigned-int31 uint31) c-type-uint31)
	(int32 c-type-int32)
	((unsigned-int32 uint32) c-type-uint32)
	((long) c-type-long)
	((unsigned-long) c-type-ulong)
	(double c-type-double)
	(fptr c-type-fptr)
	((void nil) c-type-void)
	(if-test c-type-if-test)
	(char* c-type-char-string)
	((t) c-type-lptr)
	(t (lookup-named-c-type type)))
      (cond ((eq (first type) 'const)
	     (make-c-constant-type-info :type (c-type-name->c-type-object
					       (second type))))
	    ((eq (first type) 'array)	; (array <type> dims...)
	     (make-c-array-info :element-type (c-type-name->c-type-object
					       (second type))
				:dimensions (cddr type)))
	    ((eq (second type) '*)	; pointers = arrays in C
	     (make-c-array-info :element-type (c-type-name->c-type-object
					       (first type))
				:dimensions nil))
	    (t (error "Illegal type spec: ~A" type)))))

(defun lookup-named-c-type (name)
  (or (gethash name *c-named-types*)
      (error "Unknown foreign type: ~A" name)))

(defun define-c-type-name (name type)
  (setf (gethash name *c-named-types*)
	(c-type-name->c-type-object type)))

(defun define-c-structure (info)
  ;; do defsetfs and deftype here ala defstruct
  (let ((name (c-struct-info-name info)))
    (setf (gethash name *c-named-types*) info)
    name))

(defun parse-c-struct-info (name fields)
  (make-c-struct-info :name name
		      :slots (loop for (type name) in fields
				   collect (make-c-struct-slot
					    :name name
					    :type type))))

(defun lookup-c-struct-field (info field-name)
  (or (find field-name (c-struct-info-slots info)
	    :key #'c-struct-slot-name)
      (error "C Structure ~A does not have a field named ~S"
	     (c-struct-info-name info)
	     field-name)))
