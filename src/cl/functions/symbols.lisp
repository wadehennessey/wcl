;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defmethod-inline boundp ((s symbol))
  (%boundp s))
;;; (%boundp (%32bit-ref s symbol-self-link-offset))

(defun clear-macro-name-flag (s)
  (clear-symbol-flag s macro-symbol-flag)
  s)

(defmethod clear-symbol-flag ((s symbol) (flag fixnum))
  (%bit-clear (%word-ptr (%32bit-ref s symbol-self-link-offset)
			 symbol-flags-offset)
	      flag)
  s)

(defun copy-symbol (symbol)
  (warn "write COPY-SYMBOL")
  symbol)

(defun define-function (name kind in-types out-types body safe-function
			     function)
  (setf (symbol-function name) (if (eq kind :defmethod)
				   safe-function
				   function))
  (clear-macro-name-flag name)
  name)

(defun define-variable (name value doc-string type)
  (let ((bound? (boundp name)))
    (case type
      (:var (proclaim-special-variable name)
	    (unless bound? (setf (symbol-value name) value)))
      (:parameter (proclaim-special-variable name)
		  (setf (symbol-value name) value))
      (:constant (when (and *constant-redefinition-verbose*
			    (and bound? (constant-var? name)))
		   (warn "Redefining constant ~A" name))
		 (define-constant name value)))
    name))

(defun proclaim-constant-variable (s init-form)
  (set-symbol-flag s constant-symbol-flag)
  (clear-symbol-flag s special-symbol-flag)
  (setf (get s 'constant-value) init-form)
  s)

(defun define-constant (name value)
  (proclaim-constant-variable name value)
  (setf (symbol-value name) value))

(defun constant-expr (variable)
  (let ((v (get variable 'constant-value *unbound*)))
    (if (eq v *unbound*)
	(if (boundp variable)
	    (symbol-value variable)
	    (error "Cannot find value of constant ~A" variable))
	v)))

(defun proclaimed-special? (s)
  (cond ((constant-var? s) :constant)
	((special-var? s) :special)
	(t nil)))

(defun proclaim-special-variable (s)
  (set-symbol-flag s special-symbol-flag)
  (clear-symbol-flag s constant-symbol-flag)
  s)

(defun-inline constant-var? (s)
  (= (symbol-flag s constant-symbol-flag) 1))

(defun delete-property (list indicator)
  (error "Write delete-property"))

(defmethod fboundp ((f function-specifier))
  (if (consp f)
      (%fboundp (setf-function-symbol (second f)))
      (%fboundp f)))

(defmethod fdefinition ((f function-specifier))
  (if (consp f)
      (symbol-function (setf-function-symbol f))
      (symbol-function f)))

(defmethod fmakunbound ((f function-specifier))
  (if (consp f)
      (%fmakunbound (setf-function-symbol f))
      (%fmakunbound f))
  f)

(defsetf fdefinition (function-specifier) (new-value)
  `(set-fdefinition ,new-value ,function-specifier))

(defun-inline gensym (&optional x)
  (gensym/1 x))

(defun gensym/1 (x)
  (incf  *gensym-counter*)
  (let* ((n (if (null x)
		*gensym-counter*
		(typecase x
		  (simple-string (setq *gensym-prefix* x)
				 *gensym-counter*)
		  (fixnum x))))
	 (name (string+number->string *gensym-prefix* n)))
    (make-symbol name)))

(defun-inline gentemp (&optional (prefix "T") (package *package*))
  (gentemp/2 prefix package))

(defun gentemp/2 (prefix package)
  (incf *gentemp-counter*)
  (let ((name (string+number->string prefix *gentemp-counter*)))
    (intern name package)))		; HEY! Fix to make sure it's unique

(defun string+number->string (string n)
  (let* ((strlen (%object-length string))
	 (new (fill-string-with-integer strlen 1 n)))
    (loop for i from 0 below strlen 
	  do (setf (schar new i) (schar string i)))
    new))

(defun fill-string-with-integer (extra next integer)
  (let ((quotient (%div integer 10))
	(remainder (%rem integer 10)))
    (let ((string (if (%= quotient 0)
		      (make-string (%+ extra next))
		      (fill-string-with-integer extra (%+ next 1) quotient))))
      (setf (schar string (%- (%object-length string) next))
	    (code-char (if (%> remainder 9)
			   (%+ (char-code #\A) (%- remainder 10))
			   (%+ (char-code #\0) remainder))))
      string)))

(defun get-properties (place indicator-list)
  (loop for plist on place by #'cddr
	when (member (car plist) indicator-list :test #'eq)
	do (return (values (first plist) (second plist) plist))
	finally (return (values nil nil nil))))

(defun-inline get (symbol indicator &optional default)
  (lookup-prop (symbol-plist symbol) indicator default))

(defun getf (place indicator &optional (default ()))
  (lookup-prop place indicator default))

(defun keywordp (s)
  (and (symbolp s) (eq (symbol-package s) *keyword-package*)))

(defun lookup-prop (plist indicator default)
  (loop for l on plist by #'cddr
	when (eq indicator (first l))
	do (return (second l))
	finally (return default)))

(defun macro-name? (s)
  (= (symbol-flag s macro-symbol-flag) 1))

(defun-inline make-symbol (name)
  (make_symbol name (sxhash/simple-string name)))

(defun make-static-symbol (name hash-code)
  (with-static-space (make_symbol name hash-code)))

(defmethod-inline makunbound ((s symbol))
  (%makunbound s)
  s)

(defun-inline symbol-print-escape-flag-valid? (s)
  (%= (symbol-flag s print-escape-flag-valid?) 1))

(defun-inline symbol-print-escape? (s)
  (%= (symbol-flag s print-escape-flag) 1))

(defun-inline set-symbol-print-escape-flag-valid? (s)
  (set-symbol-flag s print-escape-flag-valid?)
  s)

(defun-inline set-symbol-print-escape (s)
  (set-symbol-flag s print-escape-flag)
  s)

(defun remprop (symbol indicator)
  (remf (symbol-plist symbol) indicator))

(defun-inline set-get (symbol indicator value)
  (setf (symbol-plist symbol)
	(update-prop (symbol-plist symbol) indicator value)))

(defun set-macro-name-flag (s)
  (set-symbol-flag s macro-symbol-flag)
  s)

(defmethod set-symbol-flag ((s symbol) (flag fixnum))
  (%bit-set (%word-ptr (%32bit-ref s symbol-self-link-offset)
		       symbol-flags-offset)
	    flag)
  s)

(defmethod-inline set-symbol-function ((x symbol) (f compiled-function))
  (%symdef x symbol-function-offset f)
  f)

(defmethod-inline set-symbol-hash-code ((symbol symbol) (n fixnum))
  (%symdef symbol symbol-hashcode-offset n)
  n)

(defmethod-inline set-symbol-package ((symbol symbol) (value package))
  (%symdef symbol symbol-package-offset (if (null symbol) symbol value))
  value)

(defmethod-inline set-symbol-plist ((symbol symbol) (value list))
  (%symdef symbol symbol-plist-offset value)
  value)

;;; HEY! see type  check comment in symbol-value
(defun-inline set (x v)
  (%symdef x symbol-value-offset v)
  v)

(defun special-var? (s)
  (= (symbol-flag s special-symbol-flag) 1))

(defmethod-inline symbol-flag ((s symbol) (flag fixnum))
  (%bit-test (%32bit-ref s symbol-self-link-offset)
	     symbol-flags-offset
	     flag))

(defmethod-inline unsafe-symbol-function ((x symbol))
  (%symref x symbol-function-offset))

(defun symbol-function (symbol)
  (if (%fboundp symbol)
      (unsafe-symbol-function symbol)
      (symbol-function-error symbol)))

(defun-inline unsafe-symbol-value (symbol)
  (%symref symbol symbol-value-offset))

(defun symbol-value (symbol)
  (if (%boundp symbol)
      (unsafe-symbol-value symbol)
      (error "The variable ~A is not bound" symbol)))

(defmethod-inline symbol-hash-code ((symbol symbol))
  (%symref symbol symbol-hashcode-offset))

(defmethod-inline symbol-name ((x symbol))
  (%symref x symbol-name-offset))

(defmethod symbol-package ((symbol symbol))
  (let ((real-symbol (%32bit-ref symbol symbol-self-link-offset)))
    (if (eq real-symbol 'nil)
	*lisp-package*
	(%32bit-ref real-symbol symbol-package-offset))))

(defmethod-inline symbol-plist ((symbol symbol))
  (%symref symbol symbol-plist-offset))

(defun update-prop (plist indicator value)
  (loop for l on plist by #'cddr
	when (eq indicator (first l))
	do (progn (setf (second l) value)
		  (return plist))
	finally (return (cons indicator (cons value plist)))))

