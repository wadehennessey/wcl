;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; HEY! Need to do lots more work at EXPAND time rather than EVAL time.
;;; EXPAND should also record the macros FORM depends on
;;; and reexpand automatically if the macro def changes.
;;;
;;; At least do all decl and lambda-list parsing at expand time,
;;; figure out which vars are special, etc.

(defun eval (form)
  (eval-1 form nil nil nil nil))

(defun eval-1 (form venv fenv tenv benv)
  (let ((expansion (expand form)))
    (when (and (listp expansion)
	       (eq (car expansion) 'define-function))
      (setf (get (second (second expansion))
		 :function-definition)
	    form))
    (eval/5 expansion venv fenv tenv benv)))


;;; Return a new copy of form with all macros expanded.
(defun expand (form)
  (let ((mexp (macroexpand form *eval-macro-env*)))
    (if (atom mexp)
	mexp
	(case (first mexp)
	  ((go quote) (copy-list mexp))
	  ((unwind-protect the tagbody catch throw return-from progn
			   if setq block multiple-value-call)
	   (mapcar #'expand mexp))
	  (function (expand-function mexp))
	  (named-function (expand-named-function mexp))
	  ((flet labels) (expand-flet/labels mexp))
	  (mv-bind (expand-mv-bind mexp))
	  (macrolet (expand-macrolet mexp))
	  (symbol-macrolet (expand-symbol-macrolet mexp))
	  (eval-when (expand-eval-when mexp))
	  (define-macro (expand-define-macro mexp))
	  (t (expand-function-call mexp))))))

(defun expand-function (form)
  (destructuring-bind (f function) form
    (list f
	  (etypecase function
	    (symbol (expand function))
	    (list (expand-lambda-expr function))))))
	      
(defun expand-named-function (form)
  (destructuring-bind (f name function) form
    (list f
	  name
	  (etypecase function
	    (symbol (expand function))
	    (list (expand-lambda-expr function))))))

(defun expand-flet/labels (form)
  (destructuring-bind (name defs . body) form
    (list* name
	   (mapcar #'(lambda (d)
		       (destructuring-bind (name lambda-list . body) d
			   (list* name lambda-list (expand-body body))))
		   defs)
	   (expand-body body))))

(defun expand-mv-bind (form)
  (destructuring-bind (mv-bind vars values-form . body) form
    (list* mv-bind vars
	   (expand values-form)
	   (expand-body body))))

(defun expand-macrolet (form)
  (destructuring-bind (name macros . body) form
    (let ((*eval-macro-env*
	   (make-macro-env
	    :macros
	    (append (mapcar
		     #'(lambda (unit)
			 (destructuring-bind (name lambda-list . body) unit
			   (cons name
				 (make-macro
				  :expansion-function
				  ;; HEY! cltl2 says to do this in lex env
				  (eval (parse-macro-definition name
								lambda-list
								nil
								body))
				  :original-arg-list lambda-list))))
		     macros)
		    (macro-env-macros *eval-macro-env*))
	    :symbol-macros (macro-env-symbol-macros *eval-macro-env*))))
      (list (list* 'lambda () (expand-body body)))))) ; allow declares

(defun expand-symbol-macrolet (form)
  (destructuring-bind (name defs . body) form
      (let ((*eval-macro-env*
	     (make-macro-env
	      :macros (macro-env-macros *eval-macro-env*)
	      :symbol-macros (append defs
				     (macro-env-symbol-macros
				      *eval-macro-env*)))))
	(list (list* 'lambda () (expand-body body)))))) ; allow declares

(defun expand-eval-when (form)
  (destructuring-bind (eval-when times . body) form
    (list* 'eval-when (second form) (mapcar #'expand body))))

(defun expand-define-macro (form)
  (let ((exp (expand-function-call form)))
    ;; HEY! Cltl2 says to use current lex env...same problem with MACROLET
    (list 'quote (eval/5 exp nil nil nil nil))))

(defun expand-function-call (form)
  (destructuring-bind (function . args) form
    (let ((f (etypecase function
	       (symbol function)
	       (list (expand-lambda-expr function)))))
      (list* f (mapcar #'expand args)))))

(defun expand-lambda-expr (form)
  (destructuring-bind (lambda lambda-list . body) form
    (list* lambda lambda-list (expand-body body))))
	      
(defun expand-body (body+decls)
  (multiple-value-bind (body decls)
      (parse-body body+decls)
    (list* (cons 'declare decls) (mapcar #'expand body))))

(defun eval/5 (form venv fenv tenv benv)
  (if (atom form)
      (if (symbolp form)
	  (eval-lookup-variable-value form venv)
	  form)
      (eval-application form venv fenv tenv benv)))

(defun eval-application (form venv fenv tenv benv)
  (case (first form)
    (quote (eval-quote form))
    (if (eval-if form venv fenv tenv benv))
    (function (eval-function form venv fenv tenv benv))
    (named-function (eval-named-function form venv fenv tenv benv))
    (progn (eval-progn form venv fenv tenv benv))
    (setq (eval-setq form venv fenv tenv benv))
    (block (eval-block form venv fenv tenv benv))
    (return-from (eval-return-from form venv fenv tenv benv))
    (catch (eval-catch form venv fenv tenv benv))
    (throw (eval-throw form venv fenv tenv benv))
    (tagbody (eval-tagbody form venv fenv tenv benv))
    (go (eval-go form tenv))
    (unwind-protect (eval-unwind-protect form venv fenv tenv benv))
    ((flet labels) (eval-flet/labels form venv fenv tenv benv))
    (mv-bind (eval-mv-bind form venv fenv tenv benv))
    (multiple-value-call (eval-multiple-value-call form venv fenv tenv benv))
    (the (eval-the form venv fenv tenv benv))
    (eval-when (eval-eval-when form venv fenv tenv benv))
    (t (eval-function-call form venv fenv tenv benv))))

(defun eval-quote (form)
  (second form))

(defun eval-the (form venv fenv tenv benv)
  (eval/5 (third form) venv fenv tenv benv))

(defun eval-eval-when (form venv fenv tenv benv)
  (let ((when (second form)))
    (if (member 'eval when)
	(eval-sequence (cddr form) venv fenv tenv benv)
	nil)))

(defun eval-if (form venv fenv tenv benv)
  (if (eval/5 (second form) venv fenv tenv benv)
      (eval/5 (third form) venv fenv tenv benv)
      (eval/5 (fourth form) venv fenv tenv benv)))

(defun eval-setq (form venv fenv tenv benv)
  (loop for (var value) on (cdr form) by #'cddr
	for last = (set-variable-value var
			       (eval/5 value venv fenv tenv benv)
			       venv)
	finally (return last)))
			       

(defun eval-progn (form venv fenv tenv benv)
  (eval-sequence (cdr form) venv fenv tenv benv))

(defun eval-block (form venv fenv tenv benv)
  (let* ((block-name (second form))
	 (tag (cons block-name 'live)))
    (catch tag
      (eval-sequence (cddr form)
		     venv
		     fenv
		     tenv
		     (extend-tag-env benv block-name tag)))))

(defun eval-return-from (form venv fenv tenv benv)
  ;; HEY! Add dead block check
  (throw (eval-lookup-block-tag (second form) benv)
         (eval/5 (third form) venv fenv tenv benv)))

(defun eval-catch (form venv fenv tenv benv)
  (catch (eval/5 (second form) venv fenv tenv benv)
    (eval-sequence (cddr form) venv fenv tenv benv)))

(defun eval-throw (form venv fenv tenv benv)
  (throw (eval/5 (second form) venv fenv tenv benv)
         (eval/5 (third form) venv fenv tenv benv)))

(defun eval-tagbody (form venv fenv tenv benv)
  (let ((catch-tag (cons 'tagbody 'live))
	(body (cdr form)))
    (do ((x body (cdr x))
	 (new-tenv tenv (if (symbolp (car x))
			    (cons (list (car x) catch-tag (cdr x))
				  new-tenv)
			    new-tenv)))
	((null x)
	 (loop (when (null body)	; Ugly, but...
		 (return nil))
	       (setq body (catch catch-tag
			    (dolist (form body nil)
			      (unless (atom form)
				(eval/5 form venv fenv new-tenv benv))))))))))

(defun eval-go (form tenv)
  (let* ((entry (eval-lookup-tag-tag (second form) tenv))
	 (tag (first entry))
	 (new-body (second entry)))
    ;; HEY! Add dead tag check
    (throw tag new-body)))

(defun eval-unwind-protect (form venv fenv tenv benv)
  (unwind-protect (eval/5 (second form) venv fenv tenv benv)
    (eval-sequence (cddr form) venv fenv tenv benv)))

(defun eval-sequence (forms venv fenv tenv benv)
  (loop for rest on forms by #'cdr
	when (null (cdr rest))
	return (eval/5 (car rest) venv fenv tenv benv)
	do (eval/5 (car rest) venv fenv tenv benv)))

(defun eval-flet/labels (form venv fenv tenv benv)
  (destructuring-bind (name defs . body+decls) form
    (multiple-value-bind (body decls)
	(parse-body body+decls)
      (let ((new-fenv fenv))
	(dolist (def (reverse defs))
	  (push (cons (first def) 0) new-fenv))
	(let ((def-fenv (if (eq name 'labels) new-fenv fenv)))
	  (mapc #'(lambda (def entry)
		    (setf (cdr entry)
			  (eval/5 `(function (lambda ,@(cdr def)))
				  venv def-fenv tenv benv)))
		defs
		new-fenv)
	  (eval-sequence body venv new-fenv tenv benv))))))

(defun eval-named-function (form venv fenv tenv benv)
  (eval-lambda (second form) (third form) venv fenv tenv benv))

(defun eval-function (form venv fenv tenv benv)
  (let ((f (second form)))
    (typecase f
      (symbol (eval-lookup-function-value f fenv))
      (list (eval-lambda f f venv fenv tenv benv))
      (t (error "~S is not a function" f)))))

(defun eval-lambda (name lambda-expr venv fenv tenv benv)
  (multiple-value-bind (body decls) 
      (parse-body (cddr lambda-expr))
    (let* ((formal-args (second lambda-expr))
	   (specials
	    (loop with local-specials = (loop for (type . stuff) in decls
					      when (eq type 'special)
					      appending stuff)
		  for next in formal-args
		  as arg = (typecase next
			     (symbol next)
			     (list (let ((e (car next)))
				     (if (listp e) (second e) e)))
			     (t (error "~A is not legal in a lambda-list"
				       next)))
		  when (constant-var? arg)
		  do (error
		      "The constant ~A is not allowed in a lambda-list" arg)
		  when (or (memq arg local-specials) (special-var? arg))
		  collect arg)))
      (let ((arg-list (if (and (null (intersection formal-args
						   lambda-list-keywords))
			       (null specials))
			  formal-args
			  (let ((lambda-list (parse-lambda-list formal-args)))
			    (setf (lambda-list-specials lambda-list) specials)
			    lambda-list))))
	;; We do this part in C to make the debugger interface simpler.
	;; We could (and once did) do it all in Lisp.
	(make_eval_closure name
			   arg-list
			   body			 
			   venv
			   fenv
			   tenv
			   benv)))))

(defun coerce-to-keyword (s)
  (intern (symbol-name s) *keyword-package*))

(defun parse-lambda-list (l)
  (parse-lambda-list/4 l
		       l
		       '&required '
		       (&optional &rest &restv &key &allow-other-keys &aux)
		       (make-lambda-list)))

(defun parse-lambda-list/4 (orig rest current order info)
  (if (null rest)
      (progn (setf (lambda-list-requireds info)
		   (nreverse (lambda-list-requireds info)))
	     (setf (lambda-list-optionals info)
		   (nreverse (lambda-list-optionals info)))
	     (setf (lambda-list-auxes info)
		   (nreverse (lambda-list-auxes info)))
	     info)
      (let* ((next (car rest))
	     (new-order (member next order :test #'eq)))
	(if (null new-order)
	    (if (member next lambda-list-keywords)
		(error "Keyword ~A is out of order or repeated ~
                        in the lambda list ~A"
		       next orig)
		(progn
		  (case current
		    (&required (push next
				     (lambda-list-requireds info)))
		    (&optional (push (if (atom next) (list next nil) next)
				     (lambda-list-optionals info)))
		    (&rest (if (null (cdr (lambda-list-rest info)))
			       (setf (lambda-list-rest info) next)
			       (error "Too many &REST args ~A" rest)))
		    (&restv (if (null (cdr (lambda-list-restv info)))
				(setf (lambda-list-restv info) next)
				(error "Too many &RESTV args ~A" rest)))
		    (&key (push (cond ((atom next)
				       (list (list (coerce-to-keyword next)
						   next)
					     nil))
				      ((atom (car next))
				       (list* (list (coerce-to-keyword
						     (car next))
						    (car next))
					      (cdr next)))
				      (t next))
				(lambda-list-keys info)))
		    (&aux (push next
			  (lambda-list-auxes info)))
		    (&allow-other-keys
		     (setf (lambda-list-allow-other-keys? info) t)))
		  (parse-lambda-list/4 orig (cdr rest) current order info)))
	    (parse-lambda-list/4 orig
				 (cdr rest)
				 (car new-order)
				 (cdr new-order)
				 info)))))

(defun eval-function-call (form venv fenv tenv benv)
  (let ((function (let ((f (first form)))
		    (if (symbolp f)
			(let ((entry (assoc f fenv)))
			  (if (null entry)
			      (if (%fboundp f)
				  (symbol-function f)
				  (error "The function ~A is not defined" f))
			      (cdr entry)))
			(eval-function
			 `(function ,(first form))
			 venv fenv tenv benv))))
        (evaled-args (loop for arg in (rest form)
			   collect (eval/5 arg venv fenv tenv benv))))
    (eval-apply function evaled-args)))

(defun eval-multiple-value-call (form venv fenv tenv benv)
  (loop with func = (eval/5 (second form) venv fenv tenv benv)
	for arg in (cddr form)
	nconcing (multiple-value-list (eval/5 arg venv fenv tenv benv))
	into arg-values
	finally (return (eval-apply func arg-values))))

(defun eval-mv-bind (form venv fenv tenv benv)
  (destructuring-bind (ignore vars values-form . body+decls) form
    (declare (ignore ignore))
    (multiple-value-bind (body decls)
	(parse-body body+decls)
      (if (assoc 'special decls :test #'eq)
	  (eval-multiple-value-call
	   `(multiple-value-call #'(lambda ,vars ,@body+decls) ,values-form)
	   venv fenv tenv benv)
	  (let* ((values (multiple-value-list
			     (eval/5 values-form venv fenv tenv benv)))
		 (new-venv (eval-simple-extend-var-env
			    venv 
			    vars
			    (loop for var in vars collect (pop values)))))
	    (eval-sequence body new-venv fenv tenv benv))))))

(defun eval-lookup-variable-value (symbol venv)
  (let ((entry (assoc symbol venv)))
    (if (null entry)
        (if (boundp symbol)
	    (symbol-value symbol)
	    (error "The symbol ~S is unbound" symbol))
        (cdr entry))))

(defun eval-lookup-function-value (symbol fenv)
  (let ((entry (assoc symbol fenv)))
    (if (null entry)
        (if (%fboundp symbol)
	    (symbol-function symbol)
	    (error "The function ~S is undefined" symbol))
        (cdr entry))))

(defun set-variable-value (symbol value venv)
  (let ((entry (assoc symbol venv)))
    (if (null entry)
        (setf (symbol-value symbol) value)
        ;; destructively modify the venvironment
        (setf (cdr entry) value))))

(defun set-function-value (symbol value fenv)
  (let ((entry (assoc symbol fenv)))
    (if (null entry)
	(setf (symbol-function symbol) value)
	;; destructively modify the venvironment
	(setf (cdr entry) value))))

(defun eval-lookup-block-tag (symbol benv)
  (let ((entry (assoc symbol benv)))
    (if (null entry)
	(error "A block named ~A is not lexically visible" symbol)
	(cdr entry))))

(defun eval-lookup-tag-tag (symbol tenv)
  (let ((entry (assoc symbol tenv)))
    (if (null entry)
	(error "A tag named ~A is not lexically visible" symbol)
	(cdr entry))))

(defun eval-simple-extend-var-env (current-env new-vars new-vals)
  (let ((expected (length new-vars))
	(actual (length new-vals)))
    (if (= expected actual)
	(pairlis new-vars new-vals current-env)
	(wna expected actual))))

;;; HEY! This needs &allow-other-keys and wna-high/low checking.
(defun eval-hairy-extend-var-env (venv lambda-list args fenv tenv benv)
  (let* ((requireds (lambda-list-requireds lambda-list))
	 (rest (lambda-list-rest lambda-list))
	 (restv (lambda-list-restv lambda-list))
	 (expected (length requireds))
	 (actual (length args))
	 (specials (lambda-list-specials lambda-list))
	 (special-values (mapcar #'symbol-value specials)))
    (macrolet ((hairy-bind (var value)
		 `(if (memq ,var specials)
		   (setf (symbol-value ,var) ,value)
		   (push (cons ,var ,value) venv))))
      (when (< actual expected)
	(wna expected actual))
      (dolist (r requireds)
	(hairy-bind r (pop args)))
      (dolist (opt (lambda-list-optionals lambda-list))
	(let ((not-supplied? (null args)))
	  (hairy-bind (car opt)
		      (if not-supplied?
			  (eval/5 (second opt) venv fenv tenv benv)
			  (pop args)))
	  (unless (null (cddr opt))
	    (hairy-bind (third opt) (not not-supplied?)))))
      (unless (null rest)
	;; COPY-LIST is used because the ARGS list has dynamic-extent
	(hairy-bind rest (copy-list args)))
      (unless (null restv)
	(hairy-bind restv (coerce-to-vector args)))
      (loop for ((key var) init . supplied) in (lambda-list-keys lambda-list)
	    do (let ((found? (member key args)))
		 (hairy-bind var 
			     (if found?
				 (second found?)
				 (eval/5 init venv fenv tenv benv)))
		 (unless (null supplied)
		   (hairy-bind (car supplied) (if found? t nil)))))
      (dolist (aux (lambda-list-auxes lambda-list))
	(if (listp aux)
	    (hairy-bind (car aux) (eval/5 (second aux) venv fenv tenv benv))
	    (hairy-bind aux nil)))
      (values venv specials special-values))))

(defun eval-undo-special-bindings (specials special-values)
  (loop for old-value in special-values
	for x in specials
	do (setf (symbol-value x) old-value)))

(defun extend-tag-env (current-env new-tag new-val)
  (acons new-tag new-val current-env))

(defun wna (expected actual)
  (error "Wrong number of arguments. ~D expected, ~D supplied."
	 expected actual))

	  
	  

