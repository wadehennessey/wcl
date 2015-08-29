;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defmacro defmacro (name lambda-list &body body)
  `(define-macro ',name
    ,(parse-macro-definition name lambda-list nil body))) 

(defmacro deftype (name lambda-list &body body)
  `(define-type
    ',name
    ,(parse-macro-definition name lambda-list '* body))) 

(defmacro define-compiler-macro (name lambda-list &body body)
  `(define-compiler-macro-1 ',name
    ,(parse-macro-definition name lambda-list nil body))) 

(defmacro check-arg-type (arg type position)
  `(unless (typep ,arg ,type)
     (wta ,arg ,type ,position))) 

(defmacro defmethod (name lambda-list &body body)
  (loop for v in lambda-list
	for rest on lambda-list by #'cdr
	collect (if (listp v) (first v) v) into requireds
	collect (if (listp v) (second v) t) into in-types
	when (or (null (cdr rest))
		 (member v lambda-list-keywords :test #'eq))
	return (let ((real-lambda-list (append requireds (cdr rest)))
		     (real-body (wrap-in-block name body)))
		 `(define-function ',name :defmethod ',in-types 'nil
		   ',real-body
		   (named-function ,name
		    (lambda ,real-lambda-list
		      ,@(wrap-in-block
			 name
			 (append (mapcar #'(lambda (var type) 
					     `(check-arg-type
					       ;; HEY! fix index
					       ,var ',type 0)) 
					 requireds
					 in-types)
				 body))))
		   (named-function ,name
		    (lambda ,real-lambda-list ,@real-body)))))) 

(defmacro defun-1 (name lambda-list &body body)
  (let ((real-body (wrap-in-block name body)))
    `(define-function ',name :defun 'nil 'nil
      ',real-body
      ',nil
      (named-function ,name 
       (lambda ,lambda-list ,@real-body))))) 

(defmacro defun (name formals &body body)
  (cond ((symbolp name)
         `(defun-1 ,name ,formals ,@body))
        ((and (consp name) (eq (car name) 'setf))
         `(progn 
	   (defun-1 ,(setf-function-symbol name) ,formals ,@body)
	   (defsetf ,(second name) ,(cdr formals) (,(car formals))
	     (list ',(setf-function-symbol name) ,@formals))))
	(t (error "~A is not a legal function specifier" name)))) 

(defmacro defvar (name &optional init-form doc-string)
  `(define-variable ',name ,init-form ,doc-string :VAR)) 

(defmacro defparameter (name init-form &optional doc-string)
  `(define-variable ',name ,init-form ,doc-string :PARAMETER)) 

(defmacro defconstant (name init-form &optional doc-string)
  `(define-variable ',name ,init-form ,doc-string :CONSTANT)) 

(defmacro setf (&rest pairs)
  `(progn
    ,@(loop  for rest on pairs by #'cddr
       collect (let ((place (first rest))
		     (value (second rest)))
		 (multiple-value-bind (tvars vals svars store access)
		     (get-setf-method place)
		   (declare (ignore access))
		   (let* ((stores (mapcar #'list svars (list value)))
			  (tmps (mapcar #'list tvars vals)))
		     `(let* (,@stores
			     ,@tmps)
		       ,store))))))) 

(defmacro define-setf-method (accessor lambda-list &body body)
  `(define-setf ',accessor
    ,(parse-macro-definition accessor lambda-list nil body))) 

(defmacro defsetf (accessor name-or-args &rest stuff)
  (if (null stuff)
      `(define-setf ',accessor ',name-or-args)
      (let ((updater  (destructuring-bind ((value-var) . body) stuff
			`(apply #'(lambda (,value-var ,@name-or-args)
				    ,@body)
			  svar
			  tvars))))
	`(define-setf ',accessor
	  #'(lambda (access env)
	      (let ((svar (gensym "S"))
		    (tvars (loop for arg in (cdr access)
				 collect (gensym "T"))))
		(values tvars
			(cdr access)
			(list svar)
			,updater
			`(,(first access) ,@tvars)))))))) 

(defsetf car set-car) 

(defsetf cdr set-cdr) 

(defsetf caar (x) (new-value)
  `(set-car (car ,x) ,new-value)) 

(defsetf cadr (x) (new-value)
  `(set-car (cdr ,x) ,new-value)) 

(defsetf cdar (x) (new-value)
  `(set-cdr (car ,x) ,new-value)) 

(defsetf cddr (x) (new-value)
  `(set-cdr (cdr ,x) ,new-value)) 

(defsetf caaar (x) (new-value)
  `(set-car (caar ,x) ,new-value)) 

(defsetf cadar (x) (new-value)
  `(set-car (cdar ,x) ,new-value)) 

(defsetf cdaar (x) (new-value)
  `(set-cdr (caar ,x) ,new-value)) 

(defsetf cddar (x) (new-value)
  `(set-cdr (cdar ,x) ,new-value)) 

(defsetf caadr (x) (new-value)
  `(set-car (cadr ,x) ,new-value)) 

(defsetf caddr (x) (new-value)
  `(set-car (cddr ,x) ,new-value)) 

(defsetf cdadr (x) (new-value)
  `(set-cdr (cadr ,x) ,new-value)) 

(defsetf cdddr (x) (new-value)
  `(set-cdr (cddr ,x) ,new-value)) 

(defsetf caaaar (x) (new-value)
  `(set-car (caaar ,x) ,new-value)) 

(defsetf cadaar (x) (new-value)
  `(set-car (cdaar ,x) ,new-value)) 

(defsetf cdaaar (x) (new-value)
  `(set-cdr (caaar ,x) ,new-value)) 

(defsetf cddaar (x) (new-value)
  `(set-cdr (cdaar ,x) ,new-value)) 

(defsetf caadar (x) (new-value)
  `(set-car (cadar ,x) ,new-value)) 

(defsetf caddar (x) (new-value)
  `(set-car (cddar ,x) ,new-value)) 

(defsetf cdadar (x) (new-value)
  `(set-cdr (cadar ,x) ,new-value)) 

(defsetf cdddar (x) (new-value)
  `(set-cdr (cddar ,x) ,new-value)) 

(defsetf caaadr (x) (new-value)
  `(set-car (caadr ,x) ,new-value)) 

(defsetf cadadr (x) (new-value)
  `(set-car (cdadr ,x) ,new-value)) 

(defsetf cdaadr (x) (new-value)
  `(set-cdr (caadr ,x) ,new-value)) 

(defsetf cddadr (x) (new-value)
  `(set-cdr (cdadr ,x) ,new-value)) 

(defsetf caaddr (x) (new-value)
  `(set-car (caddr ,x) ,new-value)) 

(defsetf cadddr (x) (new-value)
  `(set-car (cdddr ,x) ,new-value)) 

(defsetf cdaddr (x) (new-value)
  `(set-cdr (caddr ,x) ,new-value)) 

(defsetf cddddr (x) (new-value)
  `(set-cdr (cdddr ,x) ,new-value)) 

(defsetf first set-car) 

(defsetf second (x) (new-value)
  `(set-car (cdr ,x) ,new-value)) 

(defsetf third (x) (new-value)
  `(set-car (cddr ,x) ,new-value)) 

(defsetf fourth (x) (new-value)
  `(set-car (cdddr ,x) ,new-value)) 

(defsetf fifth (x) (new-value)
  `(set-car (cddddr ,x) ,new-value)) 

(defsetf sixth (x) (new-value)
  `(set-car (nthcdr 5 ,x) ,new-value)) 

(defsetf seventh (x) (new-value)
  `(set-car (nthcdr 6 ,x) ,new-value)) 

(defsetf eighth (x) (new-value)
  `(set-car (nthcdr 7 ,x) ,new-value)) 

(defsetf ninth (x) (new-value)
  `(set-car (nthcdr 8 ,x) ,new-value)) 

(defsetf tenth (x) (new-value)
  `(set-car (nthcdr 9 ,x) ,new-value)) 

(defsetf nth set-nth) 

(defsetf elt set-elt) 

(defsetf aref (array &rest indices)  (new-value)
  `(set-aref ,new-value ,array ,@indices)) 

(defsetf sbit (array &rest indices)  (new-value)
  `(set-sbit ,new-value ,array ,@indices)) 

(defsetf svref set-svref) 

(defsetf oe-ref set-oe-ref) 

(defsetf schar set-schar) 

(defsetf char (string index) (new-value)
  `(set-aref ,new-value ,string ,index)) 

(defsetf 32bit-vref set-32bit-vref) 

(defsetf symbol-value set) 

(defsetf symbol-function set-symbol-function) 

(defsetf symbol-plist set-symbol-plist) 

(defsetf symbol-package set-symbol-package) 

(defsetf symbol-hash-code set-symbol-hash-code) 

(defsetf get (symbol indicator) (new-value)
  `(progn (set-get ,symbol ,indicator ,new-value)
    ,new-value)) 

(defsetf fill-pointer set-fill-pointer) 

(defsetf char-bit set-char-bit) 

(defsetf macro-function define-macro) 

(defsetf documentation set-documentation) 

(defsetf subseq (sequence start &optional (end nil)) (v)
  `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
      ,v)) 

(define-setf-method the (type place)
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place)
      (values dummies
	      vals
	      newval
	      (subst `(the ,type ,(car newval)) (car newval) setter)
	      `(the ,type ,getter)))) 

;;; This is from SPICE
(define-setf-method getf (place prop &optional default &environment env)
  (multiple-value-bind (temps values stores set get)
      (get-setf-method place env)
    (let ((newval (gensym))
	  (ptemp (gensym))
	  (def-temp (gensym)))
      (values `(,@temps ,(car stores) ,ptemp ,@(if default `(,def-temp)))
	      `(,@values ,get ,prop ,@(if default `(,default)))
	      `(,newval)
	      `(progn (setq ,(car stores)
		       (update-prop ,(car stores) ,ptemp ,newval))
		,set
		,newval)
	      `(getf ,(car stores) ,ptemp ,@(if default `(,def-temp))))))) 

(defmacro incf (ref &optional (delta 1))
  `(setf ,ref (+ ,ref ,delta))) 

(defmacro decf (ref &optional (delta 1))
  `(setf ,ref (- ,ref ,delta))) 

;;; SPICE code. Rewrite to call DELETE-PROPERTY.
(defmacro remf (place indicator &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (local1 (gensym))
	  (local2 (gensym)))
	 ((null d)
	  (push (list (car newval) getter) let-list)
	  (push (list ind-temp indicator) let-list)
	  `(let* ,(nreverse let-list)
	     (do ((,local1 ,(car newval) (cddr ,local1))
		  (,local2 nil ,local1))
		 ((atom ,local1) nil)
	       (cond ((atom (cdr ,local1))
		      (error "Odd-length property list in REMF."))
		     ((eq (car ,local1) ,ind-temp)
		      (cond (,local2
			     (rplacd (cdr ,local2) (cddr ,local1))
			     (return t))
			    (t (setq ,(car newval) (cddr ,(car newval)))
			       ,setter
			       (return t))))))))
      (push (list (car d) (car v)) let-list)))) 

(define-setf-method ldb (bytespec place &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (if (and (consp bytespec) (eq (car bytespec) 'byte))
	(let ((n-size (gensym))
	      (n-pos (gensym))
	      (n-new (gensym)))(values (list* n-size n-pos dummies)
	      (list* (second bytespec) (third bytespec) vals)
	      (list n-new)
	      `(let ((,(car newval)
		      (dpb ,n-new (byte ,n-size ,n-pos)
			   ,getter)))
		,setter
		,n-new)
	      `(ldb (byte ,n-size ,n-pos) ,getter)))
	(let ((btemp (gensym))
	      (gnuval (gensym)))
	  (values (cons btemp dummies)
		  (cons bytespec vals)
		  (list gnuval)
		  `(let ((,(car newval) (dpb ,gnuval ,btemp ,getter)))
		    ,setter
		    ,gnuval)
		  `(ldb ,btemp ,getter)))))) 

(defmacro psetf (&rest args &environment env)
  (do ((a args (cddr a))
       (let-list nil)
       (setf-list nil))
      ((atom a)
       `(let* ,(nreverse let-list) ,@(nreverse setf-list) nil))
    (if (atom (cdr a))
	(error "Odd number of args to PSETF."))
    (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method (car a) env)
      (declare (ignore getter))
      (do* ((d dummies (cdr d))
	    (v vals (cdr v)))
	   ((null d))
	(push (list (car d) (car v)) let-list))
      (push (list (car newval) (cadr a)) let-list)
      (push setter setf-list)))) 

(defmacro push (value-form var)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
      (setf ,var (cons ,value ,var))))) 

(defmacro return (&optional (value nil))
  `(return-from nil ,value)) 

(defmacro when (pred &rest args)
  `(if ,pred
    (progn ,@args)
    nil)) 

(defmacro unless (pred &rest args)
  `(if (not ,pred)
    (progn ,@args)
    nil)) 

(defmacro psetq (&rest vars+vals)
  (let* ((vars (every-even vars+vals))
	 (vals (every-odd vars+vals))
	 (tmps (n-list (length vars) #'(lambda () (gensym "TMP")))))
    `(let ,(mapcar #'list tmps vals)
      (setq ,@(mapcan #'list vars tmps))
      nil))) 


;;; HEY! change to use (end . result) no the destructuring-bind works.
(defmacro do (step-forms (end &rest result) &body decls+body)
  (let ((vars (mapcar #'(lambda (x)
			  (if (atom x) x (first x)))
		      step-forms))
	(inits (mapcar #'(lambda (x)
			   (if (atom x) nil (second x))) step-forms))
	(test-label (gensym "TEST"))
	(loop-label (gensym "LOOP")))
    (multiple-value-bind (body decls)
	(parse-body decls+body)
      `(block nil
	(let ,(mapcar #'list vars inits)
	  (declare ,@decls)
	  (tagbody (go ,test-label)	; loop inversion
	     ,loop-label
	     (psetq ,@(mapcan #'(lambda (unit)
				  (if (or (atom unit)
					  (null (cddr unit))) ; no step form?
				      nil
				      (list (first unit) (third unit))))
			      step-forms))
	     ,test-label
	     (if ,end
		 (return (progn ,@result)))
	     ,@body
	     (go ,loop-label)))))))  

;;; HEY! Unify with above?
(defmacro do* (step-forms (end &rest result) &body decls+body)
  (let ((vars (mapcar #'(lambda (x)
			  (if (atom x) x (first x)))
		      step-forms))
	(inits (mapcar #'(lambda (x)
			   (if (atom x) nil (second x))) step-forms))
	(test-label (gensym "TEST"))
	(loop-label (gensym "LOOP")))
    (multiple-value-bind (body decls)
	(parse-body decls+body)
      `(block nil
	(let* ,(mapcar #'list vars inits)
	  (declare ,@decls)
	  (tagbody (go ,test-label)	; loop inversion
	     ,loop-label
	     (setq ,@(mapcan #'(lambda (unit)
				 (if (or (atom unit)
					 (null (cddr unit))) ; no step form?
				     nil
				     (list (first unit) (third unit))))
			     step-forms))
	     ,test-label
	     (if ,end
		 (return (progn ,@result)))
	     ,@body
	     (go ,loop-label))))))) 

(defmacro dotimes ((var limitform &optional result) &body body)
  `(loop for ,var from 0 below ,limitform do (locally ,@body)
    finally (return ,result))) 

					;  (let ((limit (gensym "LIMIT")))
					;    `(do ((,limit ,limitform)
					;	  (,var 0 (+ ,var 1)))
					;      ((= ,var ,limit) ,result)
					;      (declare (fixnum ,limit ,var))
					;      ,@body)))


(defmacro dolist ((var listform &optional (result nil)) &body body)
  `(loop for ,var in ,listform do (locally ,@body) finally (return ,result))) 

(defmacro prog2 (first second &body body)
  (let ((ignore (gensym "TMP"))
	(value (gensym "VALUE")))
    `(let ((,ignore ,first))
      (let ((,value ,second))
	,@body
	,value)))) 

(defmacro prog (var-list &body body+decls)
  (multiple-value-bind (body decls)
      (parse-body body+decls)
    `(block nil
      (let ,var-list
	(declare ,@decls)
	(tagbody ,@body))))) 

(defmacro prog* (var-list &body body+decls)
  (multiple-value-bind (body decls)
      (parse-body body+decls)
    `(block nil
      (let* ,var-list
	(declare ,@decls)
	(tagbody ,@body))))) 

(defmacro and (&rest args)
  (if (null args)
      t
      (if (null (rest args))
	  (first args)
	  `(if ,(first args)
	    (and ,@(rest args))
	    nil)))) 

(defmacro or (&rest args)
  (if (null args)
      nil
      (if (null (rest args))	
	  (macroexpand (first args))
	  (let ((arg (gensym "G")))
	    `(let ((,arg ,(first args)))
	      (if ,arg
		  ,arg
		  (or ,@(rest args)))))))) 


(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
      (let* ((clause (first clauses))
	     (test (first clause))
	     (body (rest clause)))
	(if (null body)
	    (let ((var (gensym "TEST")))
	      `(let ((,var ,test))
		(if (null ,var)
		    (cond ,@(rest clauses))
	            ,var)))
	    `(if ,test
	      (progn ,@body)
	      (cond ,@(rest clauses))))))) 

(defmacro multiple-value-bind (lambda-list values-form &body body)
  `(mv-bind ,lambda-list ,values-form ,@body)) 

(defmacro multiple-value-list (values-form)
  `(multiple-value-call #'(lambda (&rest l) l) ,values-form)) 

(defmacro multiple-value-setq (vars form)
  (let ((tmps (mapcar #'(lambda (x)
			  (declare (ignore x))
			  (gensym "TMP")) vars)))
    `(multiple-value-call #'(lambda (&optional ,@tmps)
			      ,@(loop for v in vars
				      for tmp in tmps
				      collect `(setq ,v ,tmp)))
      ,form))) 
      
;;; HEY! This would be more efficient as a special form
(defmacro multiple-value-prog1 (first-form &rest other-forms)
  (let ((value-holder (gensym "MV")))
    `(multiple-value-call #'(lambda (&rest ,value-holder)
			      (progn ,@other-forms 
				     (values-list ,value-holder)))
      ,first-form))) 

(defmacro select (key-form &rest cases)
  (let ((key (gensym "KEY")))
    `(let ((,key ,key-form))
      (cond ,@(loop for (case . consequent) in cases
		    collect (cons (if (member case '(t otherwise))
				      t
				      (if (atom case)
					  `(eql ,key ,case)
					  `(or ,@(loop for c in case
						  collect `(eq ,key ,c)))))
				  consequent)))))) 

(defmacro case (key-form &rest cases)
  (let ((key (gensym "KEY")))
    `(let ((,key ,key-form))
      (cond ,@(loop for (case . consequent) in cases
		    collect (cons (if (member case '(t otherwise))
				      t
				      (list (if (atom case)
						'eql
						'member)
					    key
					    `(quote ,case)))
				  consequent)))))) 

(defmacro ecase (key &rest cases)
  `(case ,key
    ,@cases
    (t (error "~S is not one of the following constants:~{ ~A~}"
	,key
	',(collect-cases cases))))) 
	

(defmacro typecase (key &rest cases)
  (let ((k (gensym "KEY")))
    `(let ((,k ,key))
      ,(if (and (eq (caar cases) t)	; single T case?
		(null (cdr cases)))
	   `(progn ,@(cdar cases))
	   `(cond ,@(loop for (type . consequent) in cases
		     collect (if (member type '(t otherwise))
				 `(t ,@consequent)
				 `((typep ,k ',type) ,@consequent)))))))) 

(defmacro etypecase (key &rest cases)
  (let ((k (gensym "KEY")))
    `(let ((,k ,key))
      (typecase ,k
	,@cases
	(t (error "~S is not one of these types:~{ ~A~}"
		  ,k
		  ',(collect-cases cases))))))) 	

(defmacro with-open-file ((stream name &rest options) &body body)
  `(let ((,stream nil))
    (unwind-protect (progn (setq ,stream (open  ,name ,@options))
			   ,@body)
      (unless (null ,stream)
	(close ,stream))))) 

(defmacro with-open-stream ((var stream) &body forms)
  (let ((abortp (gensym)))
    `(let ((,var ,stream)
	   (,abortp t))
       (unwind-protect
	 (multiple-value-prog1
	  (progn ,@forms)
	  (setq ,abortp nil))
	 (when ,var
	   (close ,var :abort ,abortp)))))) 

(defmacro shiftf (&rest args &environment env)
  (if (< (length args) 2)
      (error "Too few argument forms to a SHIFTF."))
  (let ((leftmost (gensym)))
    (do ((a args (cdr a))
	 (let-list nil)
	 (setf-list nil)
	 (next-var leftmost))
	((atom (cdr a))
	 (push (list next-var (car a)) let-list)
	 `(let* ,(nreverse let-list) ,@(nreverse setf-list) ,leftmost))
      (multiple-value-bind (dummies vals newval setter getter)
	(get-setf-method (car a) env)
	(do* ((d dummies (cdr d))
	      (v vals (cdr v)))
	     ((null d))
	  (push (list (car d) (car v)) let-list))
	(push (list next-var getter) let-list)
	(push setter setf-list)
	(setq next-var (car newval)))))) 

(defmacro rotatef (&rest args &environment env)
  "Takes any number of SETF-style place expressions.  Evaluates all of the
  expressions in turn, then assigns to each place the value of the form to
  its right.  The rightmost form gets the value of the leftmost.  Returns NIL."
  (cond ((null args) nil)
	((null (cdr args)) `(progn ,(car args) nil))
	(t (do ((a args (cdr a))
		(let-list nil)
		(setf-list nil)
		(next-var nil)
		(fix-me nil))
	       ((atom a)
		  (rplaca fix-me next-var)
		  `(let* ,(nreverse let-list) ,@(nreverse setf-list) nil))
	       (multiple-value-bind (dummies vals newval setter getter)
                 (get-setf-method (car a) env)
		 (do ((d dummies (cdr d))
		      (v vals (cdr v)))
		     ((null d))
		   (push (list (car d) (car v)) let-list))
		 (push (list next-var getter) let-list)
		 ;; We don't know the newval variable for the last form yet,
		 ;; so fake it for the first getter and fix it at the end.
		 (unless fix-me (setq fix-me (car let-list)))
		 (push setter setf-list)
		 (setq next-var (car newval))))))) 

(defmacro pushnew (item place &key (test '#'eql) test-not (key '#'identity))
  `(setf ,place (adjoin/4 ,item ,place ,test ,key))) 

(defmacro declaim (&rest decl-specs)
  `(progn,@(loop for spec in decl-specs collect `(proclaim ',spec)))) 

(defmacro defun-inline (name &rest stuff)
  `(progn (declaim (inline ,name))
    (defun ,name ,@stuff))) 

(defmacro defmethod-inline (name &rest stuff)
  `(progn (declaim (inline ,name))
    (defmethod ,name ,@stuff))) 

(defmacro check-type (place type &optional string)
  (let ((value-var (gensym "CHECK-TYPE-VALUE")))
    `(let ((,value-var ,place))
      (unless (typep ,value-var ',type)
	(check-type-error ',place ,value-var ',type ,string))))) 

(defmacro assert (test-form &optional places datum &rest arguments)
  `(unless ,test-form
    (assert-error ',test-form))) 

(defmacro do-external-symbols ((var &optional (package '*package*) result-form)
			       &body body)
  (let ((type (gensym "TYPE")))
    `(progn (maphash #'(lambda (,var ,type)
			 (when (eq ,type :external)
			   ,@body))
	     (package-symbols (coerce-to-package ,package)))
      ,result-form))) 

(defmacro do-symbols ((var &optional (package '*package*) result-form)
		      &body body)
  (let ((p (gensym "PACKAGE"))
	(type (gensym "TYPE"))
	(tmp (gensym "TMP"))
	(body-function (gensym "BODY")))
    `(let ((,var nil))
      (flet ((,body-function (,var)
	       ,@body))
	(maphash #'(lambda (,tmp ,type)
		     (,body-function ,tmp))
		 (package-symbols (coerce-to-package ,package)))
	(dolist (,p (package-use-list ,package))
	  (do-external-symbols (,tmp ,p)
	    (,body-function ,tmp))))))) 


(defmacro do-all-symbols ((var &optional result-form)
			  &body body)
  (let ((next-package (gensym "PACKAGE"))
	(type (gensym "TYPE")))
    `(dolist (,next-package (list-all-packages) ,result-form)
      (maphash #'(lambda (,var ,type)
		   ,@body)
       (package-symbols ,next-package))))) 


(defmacro with-output-to-string ((var &optional string) &body body)
  `(let ((,var ,(if (null string)
		    `(make-string-output-stream)
		    `(make-string-output-stream-1 ,string nil))))
    (locally ,@body)
    ,(if (null string)
	 `(prog1 (get-output-stream-string ,var)
	   (close ,var))
	 string))) 

(defmacro with-input-from-string ((var string &key index start end) &body body)
  `(let ((,var (make-string-input-stream ,string)))
    (prog1 (locally ,@body)
      (close ,var)))) 

(defmacro print-unreadable-object ((object stream &key type identity)
				   &body body)
  `(let ((output ,stream)
         (obj ,object))
    (format output "#<")
    ,(when type '(format output "~S" (type-of obj)))
    ,(when (and type (or body identity)) '(format output " "))
    ,@body
    ,(when (and identity body) '(format output " "))
    ,(when identity '(format output "~O" (object->pointer obj)))
    (format output ">")
    nil)) 

;;; This should go into the compiler and evaluator someday.
(defmacro progv (symbols new-values &body body)
  (let ((syms (gensym "SYMS"))
	(new-vals (gensym "NEW-VALS"))
	(old-vals (gensym "OLD-VALS")))
    `(let* ((,syms ,symbols)
	    (,new-vals ,new-values)
	    (,old-vals (loop for symbol in ,syms
			     collect (unsafe-symbol-value symbol))))
      (unwind-protect
	   (progn (loop for symbol in ,syms
			for new-value in ,new-vals
			do (setf (symbol-value symbol) new-value))
		  ,@body)
	(progn (loop for symbol in ,syms
		     for old-value in ,old-vals
		     do (setf (symbol-value symbol) old-value))))))) 

