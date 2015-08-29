;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; Cannot self-compile this version - infinite recursion
;(defmacro destructuring-bind (lambda-list form &body body+decls)
;  (dbind-1 lambda-list form (cons 'locally body+decls)))

(define-macro 'destructuring-bind
    #'(lambda (whole env)
	(let ((lambda-list (second whole))
	      (form (third whole))
	      (body+decls (cdddr whole)))
	  (dbind-1 lambda-list form (cons 'locally body+decls)))))

(defun dbind-1 (pattern form-expr body)
  (let ((form (gensym "L"))
	(lambda-list (dot->&rest pattern)))
    (labels ((pull-apart (required)
	       (if (null required)
		   (let ((hairy (lambda-list-hairy-args lambda-list)))
		     (if (null hairy)
			 body
			 (hairy-dbind hairy form body)))
		   (let ((req (car required)))
		     (if (atom req)
			 `(let ((,req (pop ,form)))
			   ,(pull-apart (cdr required)))
			 (dbind-1 req
				  `(pop ,form)
				  (pull-apart (cdr required))))))))
      `(let ((,form ,form-expr))
	,(pull-apart (lambda-list-required-args lambda-list))))))
	
(defun hairy-dbind (hairy form body)
  (if (and (eq (first hairy) '&rest)	; just (&rest var) ?
	   (symbolp (second hairy))
	   (null (cddr hairy)))
      `(let ((,(second hairy) ,form)) ,body) 
      `(apply #'(lambda ,hairy ,body) ,form)))
    
(defun lambda-list-required-args (list)
  (subseq list 0 (or (position-if #'(lambda (e)
                                      (member e lambda-list-keywords))
                                  list)
                     (length list))))

(defun lambda-list-hairy-args (list)
  (member-if #'(lambda (x)
                 (member x lambda-list-keywords))
             list))

(defun dot->&rest (lambda-list)
  (let ((end (last lambda-list)))
    (if (null (cdr end))
	lambda-list
	(nconc (copy-list lambda-list) (list '&rest (cdr end))))))

;;; This should be called SYNTAX-CHECK or something. ANALYZE uses it.
;;; The expansion could be made more efficient (fewer cars/cdrs)
;;; if we factor out common subexpressions.
(defmacro destructure ((vars form) &body body)
  (labels ((walk-vars (expr path)
	     (if (atom expr)
		 (if (null expr)
		     expr
		     `((,expr ,path)))
		 (append (walk-vars (car expr) `(car ,path))
			 (walk-vars (cdr expr) `(cdr ,path))))))
    (let ((f (gensym "FORM-")))
      `(let ((,f ,form))
	(let ,(walk-vars vars f) ,@body)))))


;;; Because DESTRUCTURING-BIND expands into code using the following
;;; macros, we must be sure these macro don't use DESTRUCTURING-BIND,
;;; otherwise we encounter an infinite recursion when evaluating them
;;; during a native compile.
(define-macro 'pop
    #'(lambda (whole env)
	(let* ((var (second whole))
	       (list (gensym "LIST")))
	  `(let ((,list ,var))
	    (prog1 (car ,list)
	      (setf ,var (cdr ,list)))))))

(define-macro 'prog1
    #'(lambda (whole env)
	(let ((first (second whole))
	      (body (cddr whole))
	      (value (gensym "VALUE")))
	  `(let ((,value ,first))
	    ,@body
	    ,value))))

(define-macro 'locally
    #'(lambda (whole env)
	`((lambda () ,@(cdr whole)))))

;;; LET cannot use itself when evaled...
(define-macro 'let
    #'(lambda (whole env)
	((lambda (bindings)
	   ((lambda (body+decls)
	      (multiple-value-bind (body decls)
		  (parse-body body+decls)
		(if (and (null bindings) (null decls))
		    `(progn ,@body)
		    `((lambda ,(mapcar #'(lambda (spec)
					   (if (atom spec)
					       spec
					       (first spec)))
				       bindings)
	
			(declare ,@decls)
			,@body)
		      ,@(mapcar #'(lambda (spec)
				    (if (atom spec)
					'nil
					(second spec)))
			 bindings)))))
	    (cddr whole)))
	 (second whole))))

(define-macro 'let*
    #'(lambda (whole env)
	(let ((bindings (second whole))
	      (body+decls (cddr whole)))
	  (multiple-value-bind (body decls)
	      (parse-body body+decls)
	    (if (null bindings)
		(if (null decls)
		    `(progn ,@body)
		    `(locally ,@body+decls))
		(let ((first-binding (if (atom (first bindings))
					 (list (first bindings) nil)
					 (first bindings))))
		  `((lambda ,(if (null bindings)
				 nil
				 (list (first first-binding)))
		      (let* ,(rest bindings) (declare ,@decls) ,@body))
		    ,(second first-binding))))))))

