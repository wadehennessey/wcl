;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun define-macro (name expander)
  (define-macro-function
      name expander nil *macro-expanders* #'make-macro))

(defun define-type (name expander)
  (define-macro-function
      name expander nil *type-macro-expanders* #'make-type-macro))

(defun define-compiler-macro-1 (name expander)
  (define-macro-function
      name expander nil
      *compiler-macro-expanders* #'make-compiler-macro))

;;; This should probably be inline.
(defun collect (func args)
  (do ((rest (cdr args) (cdr rest))
       (result (car args) (funcall func result (car rest))))
      ((null rest) result)))

(defun compiler-macro-function (name &optional env)
  (declare (ignore env))
  (gethash name *compiler-macro-expanders*))

(defun compiler-macroexpand-1 (form &optional local-macro-env)
  (expand-macro form *compiler-macro-expanders* local-macro-env nil nil))

(defun compiler-macroexpand (form &optional local-macro-env)
  (expand-macro form *compiler-macro-expanders* local-macro-env t nil))

(defun define-macro-function (symbol function arg-list table constructor)
  (unless (null table)			; This means we're booting the library
    (when (eq table *macro-expanders*)
      (set-macro-name-flag symbol)
      (setf (symbol-function symbol) function))
    (setf (gethash symbol table)
	  (funcall constructor
		   :expansion-function function
		   :original-arg-list arg-list)))
  symbol)

(defun define-setf (accessor updater)
  (unless (null *setf-methods*)		; This means we're booting the library
    (setf (gethash accessor *setf-methods*) updater))
  accessor)

(defun every-even (l)
  (every-n 2 l))

;;; Return every Nth element of L (for N >= 1). The odd
;;; part is that we always start with the first element.
(defun every-n (n l)
  (iterate doit ((i 1)
		 (rest l))
	   (cond ((null rest) nil)
		 ((= i 1) (cons (car rest) (doit n (cdr rest))))
		 (t (doit (1- i) (cdr rest))))))

(defun every-odd (l)
  (every-n 2 (cdr l)))

(defun expand-macro (form table menv
			  repeat? original-call-is-a-macro?)
  (if (atom form)
      (let ((def (lookup-symbol-macro-def form menv)))
	(if (null def)
	    (values form original-call-is-a-macro?)
	    (values (second def) t)))
      (if (atom (car form))
	  (let ((expander (lookup-macro-expander (car form)
						 table
						 menv)))
	    (if (null expander)
		(values form original-call-is-a-macro?)
		(let ((exp (funcall *macroexpand-hook*
				    (basic-macro-expansion-function expander)
				    form
				    menv)))
		  (if (and repeat? (not (eq form exp)))
		      (expand-macro exp table menv repeat? t)
		      (values exp t)))))
	  (values form original-call-is-a-macro?))))

(defun lookup-macro-expander (name table menv)
  (let ((local (and (not (null menv))
		    (assoc name (macro-env-macros menv) :test #'eq))))
    (if (null local)
	(gethash name table)
	(cdr local))))

(defun lookup-symbol-macro-def (name menv)
  (and (not (null menv))
       (assoc name (macro-env-symbol-macros menv) :test #'eq)))

(defun get-setf-method (access &optional env)
  (get-setf-method-1 access env nil access))

(defun get-setf-method-1 (access env macroexpanded? original)
  (etypecase access
    (symbol (let ((svar (gensym "S")))
	      (values nil nil (list svar) `(setq ,access ,svar) access)))
    (list (let ((expander (gethash (car access) *setf-methods*)))
	    (if (null expander)
		(if *delay-structure-defs?*
		    (progn (complete-delayed-defstructs)
			   (get-setf-method access env))
		    (if macroexpanded?
			(error "No SETF method found for ~A" original)
			(get-setf-method-1 (macroexpand access env)
					   env
					   t
					   access)))
		(if (symbolp expander)
		    (let ((svar (gensym "S"))
			  (tvars (loop for arg in (cdr access)
				       collect (gensym "T"))))
		      (values tvars
			      (cdr access)
			      (list svar)
			      `(,expander ,@tvars ,svar)
			      (cons (first access) tvars)))
		    (funcall expander access env)))))))

;;; HEY! Shouldn't keys default automatically default also?
(defun insert-optional-default (lambda-list default)
  (loop for x in lambda-list
	for optional? =  (or (and optional?
				  (not (member x lambda-list-keywords
					       :test #'eq)))
			     (eq x  '&optional))
	collect (if (and (not (eq x '&optional))
			 optional?
			 (symbolp x))
		    `(,x ,default)
		    x)))

(defun macro-arg-list (symbol table)
  (let ((expander (lookup-macro-expander symbol table nil)))
    (if (null expander)
	nil
	(basic-macro-original-arg-list expander))))

(defun macro-function (symbol)
  (let ((expander (lookup-macro-expander symbol *macro-expanders* nil)))
    (if (null expander)
	nil
	(basic-macro-expansion-function expander))))

(defun macroexpand-1 (form &optional local-macro-env)
  (expand-macro form *macro-expanders* local-macro-env nil nil))

(defun macroexpand (form &optional local-macro-env)
  (expand-macro form *macro-expanders* local-macro-env t nil))

;;; Call INIT-FUNC N times, returing the results in a list.
(defun n-list (n init-func)
  (if (= n 0)
      nil
      (cons (funcall init-func) (n-list (1- n) init-func))))

;;; Each name should be an object which may be coerced into
;;; a string. Return a symbol whose print-name is the concatenation
;;; of those strings.
(defun names->symbol (&rest names)
  (intern (apply #'concatenate
                 'string
                 (mapcar #'string names))))

;;; Return real body and decls
(defun parse-body (body)
  (iterate separate ((rest (if (stringp (car body)) ; discard doc string
			       (if (null (cdr body))
				   body
				   (cdr body))
			       body))
		     (decls nil))
	   (let ((form (car rest)))
	     (if (or (atom form)
		     (not (eq (car form) 'declare)))
		 (values rest		; real body
			 decls)
		 (separate (cdr rest) (append (cdr form) decls))))))

(defun parse-in/out (spec)
  (multiple-value-bind (i o)
      (if (member '=> spec :test #'eq)
	  (values (subseq spec 0 (position '=> spec))
		  (subseq spec (1+ (position '=> spec))))
	  (values spec nil))
    (values (mapcar #'first i)
	    (mapcar #'first o)
	    (mapcar #'second i)
	    (mapcar #'second o))))

;;; ADD - make &body (body decls) destructure with PARSE-BODY
(defun parse-macro-definition (name args optional-default body)
  (let ((args-without-&body (subst '&rest '&body args)))
    (multiple-value-bind (whole-arg args-without-whole)
	(if (eq (car args-without-&body) '&whole)
	    (values (second args-without-&body) (cddr args-without-&body))
	    (values (gensym "WHOLE") args-without-&body))
      (multiple-value-bind (env-arg args-without-macro-stuff)
	  (let ((env (member '&environment args-without-whole :test #'eq)))
	    (if (null env) 
		(values (gensym "ENV") args-without-whole)
		(values (second env)
			(append (upto '&environment args-without-whole)
				(cddr env)))))
	(let ((dbind-list  (if (null optional-default)
			      args-without-macro-stuff
			      (insert-optional-default
			       args-without-macro-stuff
			       `(quote ,optional-default)))))
	  `(function (lambda (,whole-arg ,env-arg)
	     #+KCL (declare (ignore ,env-arg))
	     (block ,name
	       (destructuring-bind ,@(if (null dbind-list)
					 '(nil nil)
					`(,dbind-list (cdr ,whole-arg)))
		       ,@body)))))))))

(defun quoted-constant-p (l)
  (and (listp l)
       (eq (first l) 'quote)
       (null (cddr  l))))

(defun remove-compiler-macro-expander (name)
  (remhash name *compiler-macro-expanders*))

(defun remove-macro-expander (name)
  (remhash name *macro-expanders*))

(defun remove-type-macro-expander (name)
  (remhash name *type-macro-expanders*))

(defun same-length-p (l1 l2)
  (if (eq l1 '())
      (eq l2 '())
      (if (eq l2 '())
	  nil
	  (same-length-p (cdr l1) (cdr l2)))))

(defun tree-find (e tree)
  (labels ((loopy (rest)
	     (if (atom rest)
		 (if (null rest)
		     nil
		     (eq e rest))
		 (or (loopy (car rest)) (loopy (cdr rest))))))
    (loopy tree)))

(defun upto (e l)
  (nreverse (cdr (member e (reverse l)))))

(defun walk (func l)
  (if (atom l)
      (if (null l)
	  nil
	  (funcall func l))
      (or (walk func (car l)) (walk func (cdr l)))))

(defun wrap-in-block (block-name decls+body)
  (multiple-value-bind (body decls)
      (parse-body decls+body)
    `((declare ,@decls) (block ,block-name ,@body))))
