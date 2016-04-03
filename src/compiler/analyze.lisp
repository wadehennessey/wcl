;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; This is pass 1 of the compiler. Return an analyzed tree.
(defun analyze (form &optional (*dynamic-control-points* nil))
  (let* ((*env* (make-lex-env :outermost-form form))
	 (*analysis-errors* 0)
	 (*node-id* -1)
	 (original-control-points *dynamic-control-points*)
	 (tree (analyze-1 form t argc-var-name)))
    (cond ((> *analysis-errors* 0) nil)
	  ((equal original-control-points
		  *dynamic-control-points*)
	   tree)
	  ;; Keep analyzing until all block and tag xfers are correct!
	  (t (analyze form *dynamic-control-points*)))))

;;; When tail? is true we can *immediately* return form's value.
(defun analyze-1 (form tail? mv-holder &optional parent-cons)
  (if (code-p form)
      form
      (if (config-lisp-line-numbers? *config*)
	  (let ((line (source-line (or parent-cons form))))
	    (if (null line)
		(analyze-2 form tail? mv-holder)
		(let ((*current-line* line)
		      (real-form (if (line-symbol-p form)
				     (line-symbol-symbol form)
				     form)))
		  (analyze-2 real-form tail? mv-holder))))
	  (analyze-2 form tail? mv-holder))))

(defun analyze-2 (form tail? mv-holder)
  (let* ((exp-form (macroexpand-fully form *compiler-macro-env*))
	 (tree (analyze-dispatch exp-form tail? mv-holder)))
    (unless (null tree)
      (setf (code-tail? tree) tail?)
      (setf (code-mv-holder tree) mv-holder)
      ;; Move the tmp var increment into the analyzers?
      (when (typep tree '(or proc function-call if scope-seq))
	(update-tmp-var-count 1)))
    tree))

(defun analyze-dispatch (form tail? mv-holder)
  (if (atom form)
      (analyze-atom form tail? mv-holder)
      (case (first form)
	(if (analyze-if form tail? mv-holder))
	(progn (analyze-progn form tail? mv-holder))
	(block (analyze-block form tail? mv-holder))
	(catch (analyze-catch form mv-holder))
	(quote (analyze-quote form))
	(setq (analyze-setq form tail? mv-holder))
	(return-from (analyze-return-from form))
	(throw (analyze-throw form))
	(tagbody (analyze-tagbody form))
	((flet labels)
	 (analyze-local-function-def form tail? mv-holder))
	(macrolet (analyze-macrolet form tail? mv-holder))
	(symbol-macrolet (analyze-symbol-macrolet form tail? mv-holder))
	((funcall apply)
	 (analyze-funcall/apply form tail? mv-holder))
	(go (analyze-go form))
	((named-function named-macro-function)
	 (analyze-named-function form))
	(spec-bind (analyze-spec-bind form mv-holder))
	(function (analyze-function form tail?))
	(unwind-protect
	     (analyze-unwind-protect form mv-holder))
	(mv-bind (analyze-mv-bind form tail? mv-holder))
	(multiple-value-call
	    (analyze-multiple-value-call form tail? mv-holder))
	(values (analyze-values form))
	(the (analyze-the form tail? mv-holder))
	(eval-when (analyze-eval-when form tail? mv-holder))
	(c-code (analyze-c-code form tail? mv-holder))
	(c-struct-ref (analyze-c-struct-ref form tail? mv-holder))
	(c-struct-def (analyze-c-struct-def form tail? mv-holder))
	(switch (analyze-switch form tail? mv-holder))
	(t (analyze-function-call form tail? mv-holder)))))

(defun macroexpand-fully (form menv)
  (multiple-value-bind (mexp more?)
      ;; Must call compiler-macros first!!!
      (macroexpand (compiler-macroexpand form menv) menv)
    (if (and more? (not (eq form mexp)))
	(macroexpand-fully mexp menv)
	mexp)))

(defun update-tmp-var-count (n)
  (unless (null (current-proc))
    (incf (proc-max-tmp-var-count (current-proc)) n)))

(defun analyze-c-code (form tail? mv-holder)
  (declare (ignore tail? mv-holder))
  (make-c-code :string (second form)))

(defun analyze-c-struct-ref (form tail? mv-holder)
  (declare (ignore tail? mv-holder))
  (destructuring-bind (ignore struct name field) form
    (declare (ignore ignore))
    (let ((info (lookup-named-c-type name)))
      (make-c-struct-ref :name 'c-struct-ref
			 :field (lookup-c-struct-field info field)
			 :struct-info info
			 :args (list (analyze-1 struct nil nil))))))

(defun analyze-c-struct-def (form tail? mv-holder)
  (declare (ignore tail? mv-holder))
  (destructuring-bind (ignore struct name field value) form
    (declare (ignore ignore))
    (let ((info (lookup-named-c-type name)))
      (make-c-struct-def :name 'c-struct-def
			 :field (lookup-c-struct-field info field)
			 :struct-info (lookup-named-c-type name)
			 :args (list (analyze-1 struct nil nil)
				     (analyze-1 value nil nil))))))

(defun analyze-eval-when (form tail? mv-holder)
  (destructuring-bind (ignore when . body) form
    (declare (ignore ignore))
    (when (member 'compile when)
      (eval `(progn ,@body)))
    (analyze-1 (if (member 'load when)
		   `(progn ,@body)
		   nil)
	       tail?
	       mv-holder)))

(defun analyze-quote (form)
  (destructuring-bind (ignore data) form
    (declare (ignore ignore))
    (analyze-constant data)))

(defun analyze-constant (k)
  (make-constant :data k
		 :out-type (or (type-macroexpand (type-of k)) t)))

(defun analyze-atom (atom tail? mv-holder)
  (if (or (keywordp atom)
	  (eq atom 't)
	  (eq  atom 'nil)	; specially handled by the linker -STILL???
	  (and (not (symbolp atom)) (constantp atom)))
      (analyze-constant atom)
      (let ((special? (special-var-p atom)))
 	(case special?
	  (:constant (let ((expr (constant-expr atom)))
		       (if (and (config-misc-speed-hacks? *config*)
				(constant-expression? expr))
			   (analyze-constant (constant-expression-value expr))
			   (analyze-special-ref atom tail? mv-holder))))
	  (:special (analyze-special-ref atom tail? mv-holder))
	  (t (multiple-value-bind (var through-proc?)
		 (lookup-variable atom)
	       (if (null var)
		   (progn (compiler-warn "Reference to undefined variable ~A"
					 atom)
			  (analyze-special-ref atom tail? mv-holder))
		   (new-var-ref var through-proc?))))))))

(defun constant-expression? (x)
  (or (numberp x)
      (stringp x)
      (and (symbolp x) (boundp x) (eq x (symbol-value x)))
      (quoted-constant? x)))

(defun quoted-constant? (x)
  (and (listp x) (eq (first x) 'quote)))

(defun constant-expression-value (x)
  (if (quoted-constant? x)
      (second x)
      x))

(defun analyze-special-ref (sym tail? mv-holder)
  (let ((info (get-variable-info sym)))
    (analyze-1 (if (null info)
		   `(symbol-value ',sym)
		   `(the ,(variable-info-type info) (symbol-value ',sym)))
	       tail? mv-holder)))

(defun analyze-special-def (sym value tail? mv-holder)
  (let ((info (get-variable-info sym)))
    (analyze-1 (if (null info)
		   `(set ',sym ,value)
		   `(the ,(variable-info-type info) (set ',sym ,value)))
	       tail? mv-holder)))

(defun analyze-setq (form tail? mv-holder)
  (if (null (cdr form))
      (analyze-1 'nil tail? mv-holder)
      (destructuring-bind (ignore atom value . rest) form
	(declare (ignore ignore))
	(if (null rest)
	    (let ((special? (special-var-p atom)))
	      (case special?
		(:special (analyze-special-def atom value tail? mv-holder))
		(:constant (compiler-warn "Attempt to change constant ~A"
					  atom))
		(t (multiple-value-bind (var through-proc?)
		       (lookup-variable atom)
		     (if (null var)
			 (progn
			   (compiler-warn "Assignment to undefined variable ~A"
					  atom)
			   (analyze-special-def atom value tail? mv-holder))
			 (progn (incf (var-num-defs var))
				(update-var-extent var through-proc?)
				(let ((value (analyze-1 value nil nil))
				      (type (var-definite-type var)))
				  (setf (code-out-type value) type)
				  (make-var-def
				   :var var
				   :out-type type
				   :innermost-proc (current-proc)
				   :value value))))))))
	    (analyze-1 `(progn (setq ,atom ,value) (setq ,@rest))
		       tail?
		       mv-holder)))))

(defun analyze-if (form tail? mv-holder)
  (destructuring-bind (ignore test then . else) form
    (declare (ignore ignore))
    (unless (null (cdr else))
      (compiler-warn "Extra forms in IF: ~A" form))
    (let ((then-tree (analyze-1 then tail? mv-holder (cddr form)))
	  (else-tree (analyze-1 (car else) tail? mv-holder else)))
      (if (and (config-misc-speed-hacks? *config*)
	       (listp test)
	       (member (first test) '(not null) :test #'eq))
	  (make-if :test (analyze-1 (second test) nil nil (cdr form))
		   :then else-tree
		   :else then-tree)
	  (make-if :test (analyze-1 test nil nil (cdr form))
		   :then then-tree
		   :else else-tree)))))

(defun analyze-switch (form tail? mv-holder)
  (destructuring-bind (ignore test . cases) form
    (declare (ignore ignore))
    (multiple-value-bind (specific-cases default)
	(let ((rev (reverse cases)))
	  (if (eq (caar rev) t)
	      (values (reverse (cdr rev)) `(progn ,@(cdar rev)))
	      (values cases nil)))
      (loop for (key . consequent) in specific-cases
	    collect (if (listp key) key (list key)) into keys
	    collect (analyze-1 `(progn ,@consequent) tail? mv-holder)
	    into consequents
	    finally (return (make-switch
			     :test (analyze-1 test nil nil)
			     :keys keys
			     :consequents consequents
			     :default (analyze-1 default tail? mv-holder)))))))

(defun dynamic-transfer-method? (node-id)
  (member node-id *dynamic-control-points*))

(defun add-reanalysis-info (control-point)
  (push (control-point-id control-point) *dynamic-control-points*))

(defun analyze-block (form tail? mv-holder)
  (let ((node-id (incf *node-id*)))
    (destructuring-bind (ignore name . body) form
      (declare (ignore ignore))
      (if (null body)
	  (analyze-1 'nil nil mv-holder)
	  (if (dynamic-transfer-method? node-id)
	      (analyze-dynamic-block name body mv-holder)
	      (analyze-static-block node-id name body tail? mv-holder))))))

(defun analyze-static-block (node-id name body tail? mv-holder)
  (let ((point (make-static-scope-control-point
		:name name
		:id node-id
		:c-name (lisp->c-block-name name (new-name-id))
		:tail? tail?
		:mv-holder mv-holder)))
    (letf ((lex-env-blocks *env*) (cons point (lex-env-blocks *env*)))
      (letf ((lex-env-tags *env*) (cons point (lex-env-tags *env*)))
	(let ((body (loop for x in body
			  for pos from (length body) downto 1
			  collect (analyze-1 x
					     (and (= pos 1) tail?)
					     (and (= pos 1)
						  mv-holder)))))
	  (if (null (scope-control-point-refs point))
	      (make-progn-if-needed body)
	      (progn (when (static-scope-control-point-convert? point)
		       (add-reanalysis-info point))
		     (setf (scope-control-point-parent point)
			   (make-scope-seq
			    :control-point point
			    :body body)))))))))

(defun analyze-dynamic-block (name body mv-holder)
  (let ((point (make-dynamic-block-control-point
		:name name
		:tag-name (unique-control-point-tag name)
		;;  RETURN-FROM needs this tmp mv holder
		:receive-var (genstring "dynamicblock_mv_holder"))))
    (analyze-dynamic-scope body mv-holder point)))

(defun analyze-catch (form mv-holder)
  (destructuring-bind (ignore name . body) form
    (declare (ignore ignore))
    (let ((point (make-catch-control-point
		  :name name
		  :tag-name (analyze-1 name nil nil))))
      (analyze-dynamic-scope body mv-holder point))))
			
(defun analyze-dynamic-scope (body mv-holder point)
  (if (null body)
      (analyze-1 'nil nil nil)
      (letf ((lex-env-blocks *env*) (cons point (lex-env-blocks *env*)))
	(letf ((lex-env-tags *env*) (cons point (lex-env-tags *env*)))
	  (make-scope-seq
	   :control-point point
	   :body (loop for x in body
		       for pos from (length body) downto 1
		       collect (analyze-1 x
					  nil
					  (and (= pos 1) mv-holder))))))))

(defun analyze-return-from (form)
  (destructuring-bind (ignore block &optional value) form
    (declare (ignore ignore))
    (multiple-value-bind (dest through-proc? unwind-count thru-cps)
	(lookup-block block)
      (if (null dest)
	  (compiler-warn "Reference to undefined block ~A" block)
	  (let ((ct (make-scope-control-transfer
		     :destination-point dest
		     :unwind-count unwind-count)))
	    (push ct (scope-control-point-refs dest))
	    (etypecase dest
	      (dynamic-block-control-point
	       (setf (scope-control-transfer-send-value ct)
		     (analyze-1 value
				nil
				(scope-control-point-receive-var dest))))
	      (static-scope-control-point
	       (when (or through-proc?
			 (> unwind-count 0)
			 (some #'dynamic-control-point-p thru-cps))
		 (setf (static-scope-control-point-convert? dest) t))
	       (setf (scope-control-transfer-send-value ct)
		     (analyze-1 value
				(code-tail? dest)
				(code-mv-holder dest)))))
	    ct)))))

(defun analyze-throw (form)
  (destructuring-bind (ignore tag value) form
    (declare (ignore ignore))
    (let ((throw-mv-holder (genstring "throw_mv_holder")))
      (make-scope-control-transfer
       ;; dummy control point
       :destination-point (make-dynamic-scope-control-point
			   :name tag
			   :tag-name (analyze-1 tag nil nil)
			   :receive-var throw-mv-holder)
       :send-value (analyze-1 value nil throw-mv-holder)))))

(defun analyze-tagbody (form)
  (destructuring-bind (ignore . body) form
    (declare (ignore ignore))
    (if (null body)
	(analyze-1 'nil nil nil)
	(loop for x being the elements of body
	      when (symbolp x)
	      collect
	      (let ((node-id (incf *node-id*)))
		(if (dynamic-transfer-method? node-id)
		    (make-dynamic-tag-control-point
		     :name x
		     :c-name (lisp->c-tag-name x (new-name-id))
		     :tag-name (unique-control-point-tag x))
		    (make-static-tag-control-point
		     :name x
		     :id node-id
		     :c-name (lisp->c-tag-name x (new-name-id)))))
	      into control-points and
	      when (member x tags) 
	      do (compiler-warn "Duplicate tag ~A seen in tagbody" x)
	      collect x into tags 
	      finally
	      (return
		(letf ((lex-env-tags *env*)
		       (append control-points (lex-env-tags *env*)))
		  (letf ((lex-env-blocks *env*)
			 (append control-points (lex-env-blocks *env*)))
		    (let ((body (loop with head = control-points
				      for x being the elements of body
				      collect (if (symbolp x)
						  (pop head)
						  (analyze-1 x nil nil)))))
		      (loop for p in control-points
			    when (and (static-tag-control-point-p p)
				      (static-tag-control-point-convert? p))
			    do (add-reanalysis-info p))
		      (make-tag-seq
		       :control-points control-points
		       :body body)))))))))

(defun analyze-go (form)
  (destructuring-bind (ignore tag) form
    (declare (ignore ignore))
    (multiple-value-bind (dest through-proc? unwind-count thru-cps)
	(lookup-tag tag)
      (if (null dest)
	  (compiler-warn "Reference to undefined tag ~A" tag)
	  (progn
	    (when (and (static-tag-control-point-p dest)
		       (or through-proc?
			   (> unwind-count 0)
			   (some #'dynamic-control-point-p thru-cps)))
	      (setf (static-tag-control-point-convert? dest) t))
	    (make-tag-control-transfer :destination-point dest
				       :unwind-count unwind-count))))))
	      
(defun analyze-function (form tail?)
  (destructuring-bind (ignore function) form
    (declare (ignore ignore))
    (typecase function
      (symbol (multiple-value-bind (var through-proc?)
		  (lookup-function function)
		(if (null var)
		    (analyze-1 `(symbol-function ',function) tail? nil)
		    (new-var-ref var through-proc?))))
      (lambda-expr (analyze-proc (local-proc-name) function nil))
      (t (compiler-warn "~A is not a legal function" function)))))

(defun local-proc-name (&optional (name (genstring "anon")))
  (make-symbol (concatenate 'string
			    (apply #'concatenate
				   'string
				   (mapcar #'(lambda (x)
					       (symbol-name (proc-name x)))
					   *proc-chain*))
			    "-"
			    (genstring name))))

(defun analyze-named-function (form)
  (destructuring-bind (special-form name lambda-expr) form
    (analyze-proc name
		  lambda-expr
		  (eq special-form 'named-macro-function))))

(defun analyze-spec-bind (form mv-holder)
  (destructuring-bind (i1 specs . body) form
    (declare (ignore i1))
    (let ((vars (loop for s in specs collect (lookup-variable s))))
      (make-spec-bind-seq
       :specials vars
       :values (loop for var in vars collect (new-var-ref var nil))
       :body (letf ((lex-env-blocks *env*)
		    (cons :UW-SPEC-BIND (lex-env-blocks *env*)))
	       (letf ((lex-env-tags *env*)
		      (cons :UW-SPEC-BIND (lex-env-tags *env*)))
		 (analyze-1 `(progn ,@body) nil mv-holder)))))))

(defun spec-bind-body (body vars)
  (let ((specials (collect-specials vars)))
    (if (null specials)
	`(progn ,@body)
	`(spec-bind ,(mapcar #'var-name specials)
	  ,@body))))

(defun analyze-proc (name lambda-expr macro-function?)
  (destructuring-bind (i1 lambda-list . body+decls) lambda-expr
    (declare (ignore i1))
    (multiple-value-bind (body decls)
	(parse-body body+decls)
      (letf ((lex-env-decls *env*) (new-decl-env decls))
	(flet ((initial-proc (creator)
		 (funcall creator
			  :name name
			  :c-name (lisp->c-proc-name name macro-function?)
			  :max-tmp-var-count 0
			  :vars-to-declare nil
			  :volatile ""
			  :body nil
			  :funarg-refs nil))
	       (analyze-args+body (tree)
		 ;; Need to push tree into env BEFORE analyzing the lambda-list
		 ;; in case the lambda-list contains var refs to outer names.
		 (letf  ((lex-env-variables *env*)
			 (cons tree (lex-env-variables *env*)))
		   (letf ((lex-env-functions *env*)
			  (cons tree (lex-env-functions *env*)))
		     (letf ((lex-env-blocks *env*)
			    (cons tree (lex-env-blocks *env*)))
		       (letf ((lex-env-tags *env*)
			      (cons tree (lex-env-tags *env*)))
			 (let* ((*proc-chain* (cons tree
						    *proc-chain*))
				(*name-id-counter* 0)
				(var-info (analyze-lambda-list lambda-list))
				(all-vars (var-info-all-vars var-info))
				(spec-bind-body (spec-bind-body body
								all-vars)))
			   (setf (proc-var-info tree) var-info)
			   (letf ((lex-env-variables *env*)
				  (append all-vars
					  (cons tree
						(lex-env-variables *env*))))
			     (loop for var in all-vars do
				   (setf (var-innermost-proc var) tree))
			     (setf (proc-body tree)
				   (analyze-1 `(let* ,(var-info-auxes var-info)
						,spec-bind-body)
					      t
					      argc-var-name))
			     tree))))))))
	  (if (null *proc-chain*)
	      (let ((p (initial-proc #'make-top-level-proc)))
		(setf (top-level-proc-oe-vars p) nil)
		(analyze-args+body p))
	      (let ((p (initial-proc #'make-inner-proc)))
		(setf (inner-proc-oe-refs p) nil)
		(setf (inner-proc-parent-chain p) *proc-chain*)
		(analyze-args+body p))))))))

(defun analyze-macrolet (form tail? mv-holder)
  (destructuring-bind (ignore defs . body) form
    (declare (ignore ignore))
    (let* ((macros (mapcar
		    #'(lambda (unit)
			(destructuring-bind (name lambda-list . body) unit
			  (cons name
				(make-macro
				 :expansion-function
				 (eval (parse-macro-definition name
							       lambda-list
							       nil
							       body))
				 :original-arg-list lambda-list))))
		    defs))
	   (*compiler-macro-env*
	    (make-macro-env
	     :macros (append macros (macro-env-macros *compiler-macro-env*))
	     :symbol-macros (macro-env-macros *compiler-macro-env*))))
      (analyze-1 `(locally ,@body) tail? mv-holder))))

(defun analyze-symbol-macrolet (form tail? mv-holder)
  (destructuring-bind (ignore defs . body) form
      (declare (ignore ignore))
    (let ((*compiler-macro-env*
	   (make-macro-env
	    :macros (macro-env-macros *compiler-macro-env*)
	    :symbol-macros (append defs (macro-env-symbol-macros *env*)))))
      (analyze-1 `(locally ,@body) tail? mv-holder))))

(defun analyze-local-function-def (form tail? mv-holder)
  (destructuring-bind (definer defs . main-body+decls) form
    (multiple-value-bind (main-body main-decls)
	(parse-body main-body+decls)
      (let* ((vars (mapcar #'(lambda (d)
			       (new-fvar (first d)))
			   defs))
	     (functions
	      (labels ((analyze-defs ()
			 (mapcar #'(lambda (def)
				     (destructuring-bind
					   (name lambda-list . body+decls) def
				       (multiple-value-bind (body decls)
					   (parse-body body+decls)
					 (analyze-1
					  `(named-function
					    ,(local-proc-name
					      (symbol-name name))
					    (lambda ,lambda-list
					      (declare ,@decls)
					      (block ,name ,@body)))
					  nil
					  argc-var-name))))
				 defs)))
		(ecase definer
		  (labels
		      (letf ((lex-env-functions *env*)
			     (append vars (lex-env-functions *env*)))
			(analyze-defs)))
		  (flet (analyze-defs))))))
	(make-named-local
	 :letrec? (eq definer 'labels)
	 :vars vars
	 :values functions
	 :body (letf ((lex-env-functions *env*)
		      (append vars (lex-env-functions *env*)))
		 (letf ((lex-env-decls *env*) (new-decl-env main-decls))
		   (analyze-1 `(progn ,@main-body) tail? mv-holder))))))))

(defun analyze-unwind-protect (form mv-holder)
  (destructuring-bind (ignore protected-form . cleanup-forms) form
    (declare (ignore ignore))
    (letf ((lex-env-blocks *env*) (cons :UW-PROTECT (lex-env-blocks *env*)))
      (letf ((lex-env-tags *env*) (cons :UW-PROTECT (lex-env-tags *env*)))
	(declare-vars-volatile)
	(make-unwind-protect
	 :protected-form (analyze-1 protected-form nil mv-holder)
	 :cleanup-form (analyze-1 `(progn ,@cleanup-forms)
				  nil
				  nil))))))

;;; HEY! this is a hack. Fix all this mv stuff to work more rigidly.
(defun analyze-mv-bind (form tail? mv-holder)
  (destructuring-bind (ignore vars values-form . body+decls) form
    (declare (ignore ignore))
    (analyze-multiple-value-call
     `(multiple-value-call #'(lambda ,vars ,@body+decls) ,values-form)
     tail?
     mv-holder)))

(defun analyze-multiple-value-call (form tail? mv-holder)
  (destructuring-bind (ignore function . args) form
    (declare (ignore ignore))
    (if (and (anonymous-function-expr? function) ; anonymous func?
	     (= (length args) 1))
	(analyze-inline-mv-call function (car args) tail? mv-holder)
	;; SLOW general case.
	(analyze-1 `(apply ,function ,@(mapcar #'(lambda (a)
						   `(multiple-value-list ,a))
					args))
		   tail?
		   mv-holder))))

(defun analyze-inline-mv-call (func arg tail? mv-holder)
  (destructuring-bind (i1 (i2 lambda-list . body+decls)) func
    (declare (ignore i1 i2))
    (if (null lambda-list)
	(analyze-1 `((lambda ,lambda-list ,@body+decls)) tail? mv-holder)
	(multiple-value-bind (body decls)
	    (parse-body body+decls)
	  (letf ((lex-env-decls *env*) (new-decl-env decls))
	    (let* ((mv-lambda-list (cons '&optional (remove '&optional
							    lambda-list)))
		   (var-info (analyze-lambda-list mv-lambda-list))
		   (new-mv-holder (genstring "mv_holder"))
		   (all-vars (var-info-all-vars var-info))
		   (spec-bind-body (spec-bind-body body all-vars)))
	      (make-inline-mv-call
	       :new-holder new-mv-holder
	       :var-info var-info
	       :body (letf ((lex-env-variables *env*)
			    (append all-vars (lex-env-variables *env*)))
		       (analyze-1  `(let* ,(var-info-auxes var-info)
				     ,spec-bind-body)
				   tail?
				   mv-holder))
	       :values (list (analyze-1 arg nil new-mv-holder)))))))))

(defun analyze-values (form)
  (destructuring-bind (ignore . values) form
    (declare (ignore ignore))
    (make-mvalues :args (mapcar #'(lambda (v)
				    (analyze-1 v nil nil))
				values))))

(defun analyze-progn (form tail? mv-holder)
  (destructuring-bind (ignore . body) form
    (declare (ignore ignore))
    (if (null (cdr body))
	(analyze-1 (first body) tail? mv-holder)
	(make-progn-if-needed
	 (loop for x in body
	       for pos from (length body) downto 1
	       collect (analyze-1 x
				  (and (= pos 1) tail?)
				  (and (= pos 1) mv-holder)))))))

(defun analyze-function-call (form tail? mv-holder)
  (destructuring-bind (function . args) form
    (typecase function
      (symbol (analyze-named-function-call function args tail? mv-holder))
      (lambda-expr
       (analyze-inline-function-call function args tail? mv-holder))
      (t (compiler-warn "~A is not a legal function" function)))))


(defun analyze-named-function-call (function args tail? mv-holder)
  (multiple-value-bind (var through-proc?)
      (lookup-function function)
    (if (null var)			; must be a global function call
	(let ((function-info (get-proc-info function)))
	  (cond ((and (proc-info-p function-info)
		      (config-inline-calls? *config*)
		      (inline-function? function function-info)
		      (proc-info-defined? function-info))
		 (analyze-inline-function-call
		  (proc-info-lambda-expr function-info)
		  args tail? mv-holder))
		;; HEY! Generalize this sv rewrite hack!!!!
		((and (null mv-holder)
		      (eq function 'floor))
		 (analyze-1 `(floor/1v ,@args) tail? mv-holder))
		(t (let* ((arg-trees
			   (mapcar #'(lambda (x) (analyze-1 x nil nil))
				   args)))
		     (typecase function-info
		       (primitive-info
			(make-primitive-call
			 :info function-info
			 :args arg-trees))
		       (foreign-info
			(make-foreign-call
			 :name function
			 :info function-info
			 :args arg-trees))
		       (t
			(add-ftype-info
			 (make-named-call
			  :info function-info
			  :emit-as-goto? (maybe-remove-tail-recursion function
								      tail?)
			  :name function
			  :args arg-trees))))))))
	(make-unnamed-call
	 :spread-args? nil
	 :function-form (new-var-ref var through-proc?)
	 :args (mapcar #'(lambda (x) (analyze-1 x nil nil))
		       args)))))

(defun add-ftype-info (call)
  (return-from add-ftype-info call)
  ;; HEY! fix this
  (let ((info (function-call-info call)))
    (if (null info)
	call
	(progn (loop for arg in (function-call-args call)
		     for type in (function-info-in-types info)
		     do (setf (code-out-type arg)
			      (merge-types (code-out-type arg)
					   type)))
	       ;; HEY! make this work for <> 1 value
	       (setf (code-out-type call)
		     (merge-types (code-out-type call)
				  (first (function-info-out-types info))))
					; (print-call-types call)
	       call))))

(defun print-call-types (call)
  (print (function-call-name call))
  (print (code-out-type call))
  (loop for arg in (function-call-args call)
	do (print (code-out-type arg))))

(defun merge-types (one two)
  (cond ((or (null one) (eq one t)) two)
	((or (null two) (eq two t)) one)
	(t (if (subtypep one two) one two))))

(defun merge-out-type (tree type)
  (setf (code-out-type tree)
	(merge-types (code-out-type tree) type)))

(defun maybe-remove-tail-recursion (proc-name tail?)
  (let ((proc (current-proc)))
    (if (and tail?
	     (config-remove-tail-recursion? *config*)
	     (eq proc-name (proc-name proc))
	     (not (var-info-hairy? (proc-var-info proc))))
	(progn (setf (proc-start-label proc) (genstring "START"))
	       proc)
	nil)))

(defun analyze-funcall/apply (form tail? mv-holder)
  (destructuring-bind (type function . args) form
    (if (and (eq type 'funcall)		; (funcall (function ...) ...)
	     (listp function)
	     ;; DO NOT optimize (symbol-function 'foo), just (function foo)
	     ;; We want some way to force an fcell indirection.
	     (eq (first function) 'function))
	(analyze-1 `(,(second function) ,@args)  tail? mv-holder)
	(make-unnamed-call
	 :spread-args? (if (eq type 'apply) t nil)
	 :function-form (analyze-1 function nil nil)
	 :args (mapcar #'(lambda (x) (analyze-1 x nil nil)) args)))))

(defun analyze-inline-function-call (function args tail? mv-holder)
  (destructuring-bind (ignore lambda-list . body+decls) function
    (declare (ignore ignore))
    (if (hairy-lambda-list? lambda-list)
	(analyze-hairy-inline-function-call
	 function lambda-list body+decls args tail? mv-holder)
	(analyze-simple-inline-function-call
	 lambda-list body+decls args tail? mv-holder))))

(defun analyze-simple-inline-function-call (lambda-list
					    body+decls args tail? mv-holder)
  (multiple-value-bind (body decls)
      (parse-body body+decls)
    (letf ((lex-env-decls *env*) (new-decl-env decls))
      (let* ((values (mapcar #'(lambda (arg) (analyze-1 arg nil nil))
			     args))
	     (vars (mapcar #'(lambda (var val)
			       (declare (ignore val))
			       (let ((v (new-var var t)))
				 ;; Propagate type info
				 ;; HEY! This is wrong if v is assigned to!
				 ;; Need to do this in later stages.
				 ;(setf (var-definite-type v)
				 ;      (merge-types
				 ;	(code-out-type val)
				 ;	(var-definite-type v)))
				 v))
			   lambda-list
			   values))
	     (body (letf ((lex-env-variables *env*)
			  (append vars (lex-env-variables *env*)))
		     (analyze-1 (spec-bind-body body vars)
				tail?
				mv-holder))))
	(if (null args)
	    (make-progn-if-needed body)
	    (make-named-local
	     :letrec? nil
	     :vars vars
	     :values values
	     :body body))))))

;;; Perform runtime hairy-function call work at compile time when possible.
(defun analyze-hairy-inline-function-call (function lambda-list
						    body+decls args
						    tail? mv-holder)
  (let* ((info (analyze-lambda-list lambda-list :inline-function))
	 (all-var-names (mapcar #'var-name (var-info-all-vars info))))
    (multiple-value-bind (fixed-args sequential-inits)
	(hairy->fixed-arg-list info args all-var-names)
      (if (listp fixed-args)
	  (multiple-value-bind (body decls)
	      (parse-body body+decls)
	    (analyze-1 `((lambda ,all-var-names
			   (let* ,(var-info-auxes info)
			     (declare ,@decls)
			     ,@sequential-inits
			     ,@body))
			 ,@fixed-args)
		       tail?
		       mv-holder))
	  (let ((fake-name (gensym "HAIRY-INLINE")))
	    (analyze-1 `(let ((,fake-name (function ,function)))
			 (funcall ,fake-name ,@args))
		       tail?
		       mv-holder))))))

(defun simple-keyword-arg-list (args)
  (if (null args)
      t
      (and (keywordp (first args))
	   (if (null (cdr args))
	       (compiler-warn "Illegal keyword argument list")
	       t)
	   (simple-keyword-arg-list (cddr args)))))

(defun hairy->fixed-arg-list (info args all-var-names)
  (if (and (not (null (var-info-keys info)))
	   (or (not (null (var-info-rest-var info)))
	       (not (null (var-info-restv-var info)))))
      :skip-inline
      (let ((sequential-init-forms nil))
	(flet ((parallel-init-form (opt)
		 ;; If the init-form depends on previous vars in
		 ;; the lambda-list, then we must delay the init.
		 (let ((init-form (basic-optional-init-form opt))
		       (init-form-expr (basic-optional-init-form-expression
					opt)))
		   (if (or (constant-p init-form)
			   ;; Hack to detect sequential var ref dependecies
			   (and (atom init-form-expr)
				(not (member init-form-expr all-var-names)))
			   (and (listp init-form-expr)
				(null (intersection
				       all-var-names
				       (flatten init-form-expr)))))
		       init-form
		       ;; HEY! This does not obey sequential scoping
		       ;; rules for lambda list inits.
		       (progn (push `(setq ,(var-name (basic-optional-var opt))
				      ,init-form-expr)
				    sequential-init-forms)
			      nil)))))
	  (values
	   (append (loop for r in (var-info-requireds info)
			 collect
			 (if (null args)
			     (compiler-warn "Not enough required args for ~
                                                inline call")
			     (pop args)))
		   (loop for opt in (var-info-optionals info)
			 appending
			 (cons (if (null args)
				   (parallel-init-form opt)
				   (pop args))
			       (if (optional-supplied-var opt)
				   (list (not (null args)))
				   nil)))
		   (if (simple-keyword-arg-list args)
		       (loop for key in (var-info-keys info)
			     appending
			     (let ((l (member
				       (constant-data (key-name key))
				       args)))
			       (cons (if (null l)
					 (parallel-init-form key)
					 (second l))
				     (if (key-supplied-var key)
					 (list (not (null l)))
					 nil))))
		       (return-from hairy->fixed-arg-list :skip-inline))
		   (if (null (var-info-rest-var info))
		       nil
		       `(list ,@args))
		   (if (null (var-info-restv-var info))
		       nil
		       `(vector ,@args)))
	   (nreverse sequential-init-forms))))))

(defun var-lookup (name list)
  (loop with through-proc? = nil
	for x being the elements of list
	when (and (proc-p x) (null through-proc?)) ; keep first proc
	do (setf through-proc? x)
	when (and (var-p x) (eq (var-name x) name))
	do (return (values x through-proc?))))

(defun cp-lookup (name list)
  (loop with through-proc? = nil
	for x being the elements of list
	when (and (proc-p x) (null through-proc?)) ; keep first proc
	do (setf through-proc? x)
	when (dynamic-control-point-p x)
	collect x into cps
	when (and (control-point-p x)
		  (eq (control-point-name x) name))
	do (return (values x through-proc? unwind-count cps))
	when (or (member x '(:UW-PROTECT :UW-SPEC-BIND))
		 (dynamic-control-point-p x))
	count x into unwind-count))

(defun lookup-block (name)
  (cp-lookup name (lex-env-blocks *env*)))

(defun lookup-tag (name)
  (cp-lookup name (lex-env-tags *env*)))

(defun lookup-function (name)
  (var-lookup name (lex-env-functions *env*)))

(defun lookup-variable (name)
  (var-lookup name (lex-env-variables *env*)))

(defun new-label ()
  (genstring "L"))

(defun current-proc ()
  (first *proc-chain*))

(defun top-level-proc ()
  (first (last *proc-chain*)))

;;; MIPS CC has ridiculous type coercion rules regarding
;;; volatile, so I don't think we can use it. However, I 
;;; don't think we need it since longjmp restores regs.
(defun declare-vars-volatile ()
  ;; Used to be "volatile "
  (setf (proc-volatile (current-proc)) ""))

(defun update-var-extent (var through-proc?)
  (when through-proc?
    (push var (inner-proc-oe-refs through-proc?))
    (loop for p in *proc-chain*
	  do (etypecase p
	       (top-level-proc
		(pushnew var (top-level-proc-oe-vars p)))
	       (inner-proc
		(setf (inner-proc-pass-on-oe? p) t))))
    (setf (var-extent var) :indefinite)))


(defun analyze-lambda-list (lambda-list &optional inline-function?)
  (letf ((lex-env-variables *env*) (lex-env-variables *env*)) 
    (key-list-iterate munch
	(next lambda-list
	      (let* ((req (nreverse requireds))
		     (o (nreverse optionals))
		     (k (nreverse keys))
		     (r (if (and (null rest) (not inline-function?))
			    (if (null keys)
				nil
				(let ((v (new-var (gensym "KEYS") t)))
				  (setf (var-dynamic-extent? v) t)
				  v))
			    (car rest)))
		     (rv (car restv))
		     (hairy? (or o k r rv)))
		(unless (or (null r) (null rv))
		  (compiler-warn
		   "You cannot use &REST and &RESTV at the same time"))
		(make-var-info
		 :requireds req :optionals o :keys k :rest-var r :restv-var rv
		 :auxes (nreverse auxes)
		 :hairy? hairy?
		 :all-vars (append
			    req
			    (mapcan #'list-basic-optional-vars o)
			    (mapcan #'list-basic-optional-vars k)
			    rest
			    restv)
		 :allow-other-keys? allow-other-keys?)))
	((requireds nil) (optionals nil) (rest nil)
	 (restv nil) (keys nil) (auxes nil) (allow-other-keys? nil)
	 (current '&required)
	 (order '(&optional &rest &restv &key &allow-other-keys &aux)))
      (let ((new-order (member next order :test #'eq)))
	(if (null new-order)
	    (if (member next lambda-list-keywords)
		(error "Keyword ~A is not permitted, out of order, ~
                        or repeated in the lambda list ~A"
		       next lambda-list)
		(ecase current
		  (&required (munch :requireds (cons (create-required-var next)
						     requireds)))
		  (&optional (munch :optionals (cons (create-optional next)
						     optionals)))
		  (&rest (if (null (cdr rest))
			     (munch :rest (cons (create-rest-var next)
						rest))
			     (error "Too many &REST args ~A" rest)))
		  (&restv (if (null (cdr restv))
			      (munch :restv (cons (create-rest-var next)
						  restv))
			      (error "Too many &RESTV args ~A" rest)))
		  (&key (munch :keys (cons (create-key next) keys)))
		  (&aux (munch :auxes (cons (create-aux next) auxes)))))
	    (munch :current (car new-order)
		   :order (cdr new-order)
		   :allow-other-keys? (or allow-other-keys?
					  (eq (car new-order)
					      '&allow-other-keys))))))))

(defun create-required-var (name)
  (if (symbolp name)
      (let ((v (new-var name nil)))
	(push v (lex-env-variables *env*))
	v)
      (error "~A is not legal as the name of a required variable" name)))

(defun create-rest-var (name)
  (let ((v (new-var name t)))
    (push v (lex-env-variables *env*))
    v))

(defun create-optional (optional)
  (let* ((v (new-var (if (atom optional) optional (car optional))
		     t))
	 (o (fill-basic-optional-slots optional (make-optional :var v))))
    (push-basic-opt-vars-onto-env o)
    o))

(defun create-key (key)
  (flet ((make-name (key)
	   (analyze-1 (intern (symbol-name key)
			      (find-package "KEYWORD"))
		      nil
		      nil)))
    (let ((k (cond ((atom key)
		    (make-key :var (new-var key t)
			      :name (make-name key)))
		   ((atom (car key))
		    (fill-basic-optional-slots
		     key
		     (make-key :var (new-var (car key) t)
			       :name (make-name (car key)))))
		   (t (fill-basic-optional-slots
		       key
		       (make-key :var (new-var (second (car key)) t)
				 :name (make-name (first (car key)))))))))
      (push-basic-opt-vars-onto-env k)
      k)))

(defun push-basic-opt-vars-onto-env (o)
  (push (basic-optional-var o) (lex-env-variables *env*))
  (let ((supp (basic-optional-supplied-var o)))
    (unless (null supp) (push supp (lex-env-variables *env*)))))

(defun list-basic-optional-vars (o)
  (let ((req (basic-optional-var o))
	(supp (basic-optional-supplied-var o)))
    (cons req  (if (null supp) nil (list supp)))))

(defun fill-basic-optional-slots (list struct)
  (unless (atom list)
    (setf (basic-optional-init-form-expression struct)
	  (second list))
    (setf (basic-optional-init-form struct)
	  (analyze-1 (second list) nil nil))
    (setf (basic-optional-supplied-var struct)
	  (if (null (third list))
	      nil
	      (new-var (third list) t))))
  struct)

(defun create-aux (aux)
  (if (atom aux) `(,aux nil) aux))

(defun parse-decls (decls)
  (key-list-iterate parse
      (spec decls (make-decls :specials specials
			      :types types
			      :ftypes ftypes	
			      :inlines inlines
			      :notinlines notinlines
			      :ignores ignores
			      :dynamic-extents dynamic-extents
			      :optimizes optimizes))
      ((specials nil)
       (types nil)
       (ftypes nil)
       (inlines nil)
       (notinlines nil)
       (ignores nil)
       (dynamic-extents nil)
       (optimizes nil))
    (case (car spec)
      (special (parse :specials (append (cdr spec) specials)))
      (type (parse :types (add-type-decls (second spec) (cddr spec) types)))
      (ftype (parse :ftypes (add-type-decls (second spec) (cddr spec) ftypes)))
      (function (parse :ftypes (add-type-decls `(function ,@(cddr spec))
					       (list (second spec))
					       ftypes)))
      (inline (parse :inlines (append (cdr spec) inlines)))
      (notinline (parse :notinlines (append (cdr spec) notinlines)))
      ;; HEY! make ignoreable work correctly.
      ((ignore ignoreable) (parse :ignores (append (cdr spec) ignores)))
      (dynamic-extent (parse :dynamic-extents (append (cdr spec)
						      dynamic-extents)))
      (optimize (parse :optimizes (append (cdr optimizes) ignores)))
      (t (cond ((legal-shorthand-type-decl-p (car spec))
		(parse :types (add-type-decls (car spec) (cdr spec) types)))
	       ((ok-foreign-declaration-p (car spec))
		(parse))
	       (t (progn (warn "Ignoring unknown declaration ~A"
			       spec)
			 (parse))))))))

(defun new-decl-env (decls)
  (if (null decls)
      (lex-env-decls *env*)
      (cons (parse-decls decls) (lex-env-decls *env*))))

(defun add-type-decls (type-spec vars existing-type-decls)
  (let ((expanded-type-spec (type-macroexpand type-spec)))
    (append (mapcar #'(lambda (var)
			(cons var expanded-type-spec))
		    vars)
	    existing-type-decls)))

(defun legal-shorthand-type-decl-p (type-spec)
  (member (if (atom type-spec)
	      type-spec
	      (car type-spec))
	  *standard-type-specifier-symbols*))

(defun lookup-special-decl (var decls-list)
  (dolist (decls decls-list (proclaimed-special? var))
    (when (member var (decls-specials decls))
      (return :special))))

(defun lookup-type-decl (var)
  (dolist (decls (lex-env-decls *env*) t)
    (let ((entry (assoc var (decls-types decls))))
      (unless (null entry)
	(return (cdr entry))))))

(defun lookup-dynamic-extent-decl (var)
  (dolist (decls (lex-env-decls *env*))
    (when (member var (decls-dynamic-extents decls))
      (return t))))

(defun lookup-ignore-decl (var)
  (dolist (decls (lex-env-decls *env*))
    (when (member var (decls-ignores decls))
      (return t))))

;;; HEY! This should only return true if we find an inline decl
;;; AND we actually have lambda-expr info availabe for the function!!!
;;; Don't use this until we fix it.
(defun lookup-inline-decl (func decls-list)
  (dolist (decls decls-list)
    (when (member func (decls-inlines decls))
      (return t))))

(defun lookup-notinline-decl (func decls-list)
  (dolist (decls decls-list)
    (when (member func (decls-notinlines decls))
      (return t))))

(defun inline-function? (func function-info)
  (let ((decls-list (lex-env-decls *env*)))
    (and (not (lookup-notinline-decl func decls-list))
	 (or ;; Dont use this until fixed: (lookup-inline-decl func decls-list)
	     (proc-info-inline? function-info)))))

(defun ok-foreign-declaration-p (decl)
  (member decl *ok-foreign-declarations*))

(defun analyze-the (form tail? mv-holder)
  (destructuring-bind (ignore type body) form
    (declare (ignore ignore))
    (let ((tree (analyze-1 body tail? mv-holder)))
      (setf (code-out-type tree) type)
      tree)))

(defun new-var (name declare?)
  (let ((v (make-variable-var
	    :name name
	    :c-name (lisp->c-variable-name name (new-name-id))
	    :definite-type (lookup-type-decl name)
	    :declared-ok-to-ignore? (lookup-ignore-decl name)
	    :num-refs 0
	    :num-defs 0
	    :extent :dynamic
	    :dynamic-extent? (lookup-dynamic-extent-decl name)
	    :innermost-proc (current-proc))))
    (when declare?
      (push v (proc-vars-to-declare (current-proc))))
    v))

(defun new-fvar (name)
  (let ((v (make-function-var
	    :name name
	    :c-name (lisp->c-function-name name (new-name-id))
	    :num-refs 0
	    :num-defs 0
	    :extent :dynamic
	    :dynamic-extent? (lookup-dynamic-extent-decl name)
	    :innermost-proc (current-proc))))
    (push v (proc-vars-to-declare (current-proc)))
    v))

(defun hairy-lambda-list? (lambda-list)
  (dolist (x lambda-list)
    (when (member x lambda-list-keywords :test #'eq)
      (return t))))

(defun proc-required-vars (p)
  (var-info-requireds (proc-var-info p)))

(defun collect-specials (var-list)
  (loop for var in var-list
	for spec? = (special-var-p (var-name var))
	when (eq spec? :special)
	collect var
	when (eq spec? :constant)
	do (compiler-warn "~A has been declared constant but you ~
                            are using it as a variable" (var-name var))))

(defun anonymous-function-expr? (expr)
  (and (listp expr) (listp (second expr))))

(defun lambda-expr? (l)
  (and (eq (first l) 'lambda)
       (listp (second l))))

(defun new-var-ref (var through-proc?)
  (incf (var-num-refs var))
  (update-var-extent var through-proc?)
  (make-var-ref :var var
		:out-type (var-definite-type var)
		:innermost-proc (current-proc)))

(defun new-name-id ()
  (prog1 *name-id-counter*
    (incf *name-id-counter*)))

(defun c-type->lisp-type (c-type)
  (case c-type
    (int 'fixnum)
    (double 'float)
    (char 'character)
    (if-test 't)
    (t c-type)))

(defun flatten (l)
  (if (null l)
      nil
      (if (atom (car l))
	  (cons (car l) (flatten (cdr l)))
	  (append (flatten (car l)) (flatten (cdr l))))))    

(defun dynamic-control-point-p (x)
  (typep x '(or dynamic-scope-control-point dynamic-tag-control-point)))

(defun unique-control-point-tag (name)
  (analyze-1 `(quote ,(gentemp (symbol-name name))) nil nil))

(defun make-progn-if-needed (body)
  (cond ((atom body) body)
	((null (cdr body)) (car body))
	(t (make-progn :body body))))


