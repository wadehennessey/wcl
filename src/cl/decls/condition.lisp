
;;; HEY! Fix up how conditions are defined

(defvar *break-on-signals* nil)
(defvar *break-on-warnings* nil)
(defvar *debugger-hook* nil)
(defvar *handler-clusters* nil)
(defvar *restart-clusters* '())

(defstruct (restart (:print-function restart-print))
  name
  function
  report-function
  interactive-function)

(defstruct (condition (:conc-name nil)
                      (:constructor constructor-for-condition)
                      (:predicate nil)
                      (:print-function condition-print)
		      ;; This structure may be used by other libraries,
		      ;; so we have to keep things dynamic.
		      (:dynamic t))	
  ;; Why do we need this?
  (-dummy-slot- nil))

(defmacro with-keyword-pairs ((names expression &optional keywords-var)
			      &body forms)
  (let ((temp (member '&rest names)))
    (unless (= (length temp) 2)
      (error "&REST keyword is ~:[missing~;misplaced~]." temp))
    (let ((key-vars (ldiff names temp))
	  (key-var (or keywords-var (gensym)))
	  (rest-var (cadr temp)))
      (let ((keywords (mapcar #'(lambda (x)
				  (intern (string x)
					  *keyword-package*))
			      key-vars)))
	`(multiple-value-bind (,key-var ,rest-var)
	  (parse-keyword-pairs ,expression ',keywords)
	  (let ,(mapcar #'(lambda (var keyword)
			    `(,var (getf ,key-var ,keyword)))
			key-vars keywords)
	    ,@forms))))))

(defmacro define-condition (name (parent-type) slot-specs &rest options)
  (let ((constructor (intern (format nil "CONSTRUCTOR-FOR-~S" name))))
    (let ((slots (mapcar #'(lambda (slot-spec)
			     (if (atom slot-spec) (list slot-spec) slot-spec))
			 slot-specs)))
      (multiple-value-bind (new-slots used-slots)
	  ;; HEY! fix this to allow changing init of inherited slots
	  (values slots nil)
	;;(parse-new-and-used-slots slots parent-type)
	(let ((conc-name-p     nil)
	      (conc-name       nil)
	      (report-function nil)
	      (documentation   nil))
	  (do ((o options (cdr o)))
	      ((null o))
	    (let ((option (car o)))
	      (case (car option)	;should be ecase
		(:conc-name (setq conc-name-p t)
		 	    (setq conc-name (cadr option)))
		(:report (setq report-function
			       (if (stringp (cadr option))
				   `(lambda (stream)
				     (write-string ,(cadr option) stream))
				   (cadr option))))
		(:documentation (setq documentation (cadr option)))
		(otherwise
		 (cerror "Ignore this define-condition option."
			 "Invalid define-condition option: ~S" option)))))
	  (if (not conc-name-p)
	      (setq conc-name (intern (format nil "~A-" name) *package*)))
          `(defstruct (,name
		       (:constructor ,constructor)
		       (:predicate nil)
		       (:copier nil)
		       (:print-function condition-print)
		       (:include ,parent-type ,@used-slots)
		       (:conc-name ,conc-name)
		       ;; This structure may be used by other libraries,
		       ;; so we have to keep things dynamic.
		       (:dynamic t)	
		       (:report-function ,report-function))
	    ,@new-slots))))))

(define-condition warning (condition) ())

(define-condition serious-condition (condition) ())

(define-condition error (serious-condition) ())

(defun simple-condition-printer (condition stream)
  (apply #'format stream (simple-condition-format-string    condition)
	 (simple-condition-format-arguments condition)))

(define-condition simple-condition (condition)
  (format-string (format-arguments '()))
  (:conc-name internal-simple-condition-)
  (:report simple-condition-printer))

(define-condition simple-warning (warning)
  (format-string (format-arguments '()))
  (:conc-name internal-simple-warning-)
  (:report simple-condition-printer))

(define-condition simple-error (error)
  (format-string (format-arguments '()))
  (:conc-name internal-simple-error-)
  (:report simple-condition-printer))

(define-condition storage-condition (serious-condition) ())

(define-condition stack-overflow (storage-condition) ())

(define-condition storage-exhausted (storage-condition) ())

(define-condition type-error (error) (datum expected-type))

(define-condition simple-type-error (type-error)
  (format-string (format-arguments '()))
  (:conc-name internal-simple-type-error-)
  (:report simple-condition-printer))

(define-condition case-failure (type-error)
  (name possibilities)
  (:report
   (lambda (condition stream)
     (format stream "~S fell through ~S expression.~%Wanted one of ~:s."
	     (type-error-datum condition)
	     (case-failure-name condition)
	     (case-failure-possibilities condition)))))

(defun simple-condition-format-string (condition)
  (etypecase condition
    (simple-condition  (internal-simple-condition-format-string  condition))
    (simple-warning    (internal-simple-warning-format-string    condition))
    (simple-type-error (internal-simple-type-error-format-string condition))
    (simple-error      (internal-simple-error-format-string      condition))))

(defun simple-condition-format-arguments (condition)
  (etypecase condition
    (simple-condition  (internal-simple-condition-format-arguments  condition))
    (simple-warning    (internal-simple-warning-format-arguments    condition))
    (simple-type-error (internal-simple-type-error-format-arguments condition))
    (simple-error      (internal-simple-error-format-arguments      condition))))

(define-condition program-error (error) ())

(define-condition control-error (error) ())

(define-condition stream-error (error) (stream))

(define-condition end-of-file (stream-error) ())

(define-condition file-error (error) (pathname))

(define-condition package-error (error) (pathname))

(define-condition cell-error (error) (name))

(define-condition unbound-variable (cell-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "The variable ~S is unbound."
		     (cell-error-name condition)))))

(define-condition undefined-function (cell-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "The function ~S is undefined."
		     (cell-error-name condition))))) 

(define-condition arithmetic-error (error) (operation operands))

(define-condition division-by-zero         (arithmetic-error) ())

(define-condition floating-point-overflow  (arithmetic-error) ())

(define-condition floating-point-underflow (arithmetic-error) ())

(define-condition abort-failure (control-error)
  ()
  (:report "abort failed."))

(defmacro restart-bind (bindings &body forms)
  `(let ((*restart-clusters*
	  (cons (list ,@(mapcar #'(lambda (binding)
				    `(make-restart
				      :name ',(car binding)
				      :function ,(cadr binding)
				      ,@(cddr binding)))
				bindings))
		*restart-clusters*)))
    ,@forms))

(defmacro restart-case (expression &body clauses)
  (flet ((transform-keywords (&key report interactive)
	   (let ((result '()))
	     (when report
	       (setq result (list* (if (stringp report)
				       `#'(lambda (stream)
					    (write-string ,report stream))
				       `#',report)
				   :report-function
				   result)))
	     (when interactive
	       (setq result (list* `#',interactive
				   :interactive-function
				   result)))
	     (nreverse result))))
    (let ((block-tag (gensym))
	  (temp-var  (gensym))
	  (data
	   (mapcar #'(lambda (clause)
		       (with-keyword-pairs ((report interactive &rest forms)
					    (cddr clause))
			 (list (car clause)			   ;name=0
			       (gensym)			   ;tag=1
			       (transform-keywords :report report ;keywords=2
						   :interactive interactive)
			       (cadr clause)			   ;bvl=3
			       forms)))			   ;body=4
		   clauses)))
      `(block ,block-tag
	(let ((,temp-var nil))
	  (tagbody
	     (restart-bind
	      ,(mapcar #'(lambda (datum)
			   (let ((name (nth 0 datum))
				 (tag  (nth 1 datum))
				 (keys (nth 2 datum)))
			     `(,name #'(lambda (&rest temp)
					 (setq ,temp-var temp)
					 (go ,tag))
			       ,@keys)))
		       data)
	      (return-from ,block-tag ,expression))
	     ,@(mapcan #'(lambda (datum)
			   (let ((tag  (nth 1 datum))
				 (bvl  (nth 3 datum))
				 (body (nth 4 datum)))
			     (list tag
				   `(return-from ,block-tag
				     (apply #'(lambda ,bvl ,@body)
				      ,temp-var)))))
		       data)))))))


(defmacro with-simple-restart ((restart-name format-string
					     &rest format-arguments)
			       &body forms)
  `(restart-case (progn ,@forms)
    (,restart-name ()
     :report (lambda (stream)
	       (format stream ,format-string ,@format-arguments))
     (values nil t))))

(defmacro resolve-function (function expression resolver)
    `(cond ((and ,function ,expression)
	    (cerror "use only the :~A information."
	     "only one of :~A and :~A is allowed."
	     ',function ',expression))
      (,expression
       (setq ,function ,resolver))))  

(defmacro handler-bind (bindings &body forms)
  (unless (every #'(lambda (x) (and (listp x) (= (length x) 2))) bindings)
    (error "ill-formed handler bindings."))
  `(let ((*handler-clusters*
	  (cons (list ,@(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
				bindings))
		*handler-clusters*)))
    ,@forms))

(defmacro handler-case (form &rest cases)
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (make-symbol "normal-return"))
	      (error-return  (make-symbol "error-return")))
	  `(block ,error-return
	    (multiple-value-call #'(lambda ,@(cdr no-error-clause))
	      (block ,normal-return
		(return-from ,error-return
		  (handler-case (return-from ,normal-return ,form)
				,@(remove no-error-clause cases)))))))
	(let ((tag (gensym))
	      (var (gensym))
	      (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
				       cases)))
	  `(block ,tag
	    (let ((,var nil))
	      ,var				;ignorable
	      (tagbody
		 (handler-bind
		  ,(mapcar #'(lambda (annotated-case)
			       (list (cadr annotated-case)
				     `#'(lambda (temp)
					  ,@(if (caddr annotated-case)
						`((setq ,var temp)))
					  (go ,(car annotated-case)))))
			   annotated-cases)
			       (return-from ,tag ,form))
		 ,@(mapcan #'(lambda (annotated-case)
			       (list (car annotated-case)
				     (let ((body (cdddr annotated-case)))
				       `(return-from ,tag
					 ,(cond ((caddr annotated-case)
						 `(let ((,(caaddr
							   annotated-case)
							 ,var))
						   ,@body))
						((not (cdr body))
						 (car body))
						(t
						 `(progn ,@body)))))))
			   annotated-cases))))))))

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
    (error (condition) (values nil condition))))



#| 
Merge these into current macros

(defun accumulate-cases (macro-name cases list-is-atom-p)
  (do ((l '())
       (c cases (cdr c)))
      ((null c) (nreverse l))
    (let ((keys (caar c)))
      (cond ((atom keys)
	     (cond ((null keys))
		   ((member keys '(otherwise t))
		    (error "otherwise is not allowed in ~S expressions."
			   macro-name))
		   (t (push keys l))))
	    (list-is-atom-p
	     (push keys l))
	    (t
	     (dolist (key keys) (push key l)))))))

(defmacro xecase (keyform &rest cases)
  (let ((keys (accumulate-cases 'ecase cases nil))
	(var (gensym)))
    `(let ((,var ,keyform))
      (case ,var
	,@cases
	(otherwise
	 (error 'case-failure :name 'ecase
		:datum ,var
		:expected-type '(member ,@keys)
		:possibilities ',keys))))))

(defmacro xccase (keyplace &rest cases)
  (let ((keys (accumulate-cases 'ccase cases nil))
	(tag1 (gensym))
	(tag2 (gensym)))
    `(block ,tag1
      (tagbody ,tag2
	 (return-from ,tag1
	   (case ,keyplace
	     ,@cases
	     (otherwise
	      (restart-case
	       (error 'case-failure
		      :name 'ccase
		      :datum ,keyplace
		      :expected-type '(member ,@keys)
		      :possibilities ',keys)
	       (store-value
		(value)
		:report (lambda (stream)
			  (format stream "supply a new value of ~S."
				  ',keyplace))
		:interactive read-evaluated-form
		(setf ,keyplace value)
		(go ,tag2))))))))))



(defmacro xetypecase (keyform &rest cases)
  (let ((types (accumulate-cases 'etypecase cases t))
	(var (gensym)))
    `(let ((,var ,keyform))
      (typecase ,var
	,@cases
	(otherwise
	 (error 'case-failure :name 'etypecase
		:datum ,var
		:expected-type '(or ,@types)
		:possibilities ',types))))))


(defmacro ctypecase (keyplace &rest cases)
  (let ((types (accumulate-cases 'ctypecase cases t))
	(tag1 (gensym))
	(tag2 (gensym)))
    `(block ,tag1
      (tagbody ,tag2
	 (return-from ,tag1
	   (typecase ,keyplace
	     ,@cases
	     (otherwise
	      (restart-case
	       (error 'case-failure
		      :name 'ctypecase
		      :datum ,keyplace
		      :expected-type '(or ,@types)
		      :possibilities ',types)
	       (store-value
		(value)
		:report (lambda (stream)
			  (format stream "supply a new value of ~S."
				  ',keyplace))
		:interactive read-evaluated-form
		(setf ,keyplace value)
		(go ,tag2))))))))))

(defmacro assert (test-form &optional places datum &rest arguments)
  (let ((tag (gensym)))
    `(tagbody ,tag
      (unless ,test-form
	(restart-case
	 ,(if datum
	      `(error ,datum ,@arguments)
	      `(simple-assertion-failure ',test-form))
	 (continue ()
		   :report (lambda (stream) (assert-report ',places stream))
		   ,@(mapcar #'(lambda (place)
				 `(setf ,place (assert-prompt ',place ,place)))
			     places)
		   (go ,tag)))))))

(defmacro xcheck-type (place type &optional type-string)
  (let ((tag1 (gensym))
	(tag2 (gensym)))
    `(block ,tag1
      (tagbody ,tag2
	 (if (typep ,place ',type) (return-from ,tag1 nil))
	 (restart-case
	  ,(if type-string
	       `(error "the value of ~S is ~S, ~
				     which is not ~A."
		 ',place ,place ,type-string)
	       `(error "the value of ~S is ~S, ~
				     which is not of type ~S."
		 ',place ,place ',type))
	  (store-value (value)
		       :report (lambda (stream)
				 (format stream "supply a new value of ~S."
					 ',place))
		       :interactive read-evaluated-form
		       (setf ,place value)
		       (go ,tag2)))))))
|#
