;;; Originally written by Kent Pittman. This note came with the code:
;;; 
;;; This is a sample implementation. It is not in any way intended as the
;;; definition of any aspect of the condition system. It is simply an
;;; existence proof that the condition system can be implemented.
;;; 
;;; While this written to be "portable", this is not a 
;;; portable condition system in that loading this file will 
;;; not redefine your condition system. Loading this
;;; file will define a bunch of functions which work like a condition 
;;; system. Redefining existing condition systems is beyond the goal
;;; of this implementation attempt.

(defun make-condition (type &rest slot-initializations)
  (let ((fn (make-function type)))
    (cond ((not fn) (error 'simple-type-error
			   :datum type
			   :expected-type '(satisfies make-function)
			   :format-string "not a condition type: ~S"
			   :format-arguments (list type)))
	  (t (apply fn slot-initializations)))))

(defun compute-restarts ()
  (copy-list (apply #'append *restart-clusters*)))

(defun restart-print (restart stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<Restart ~S ~X>"
	      (type-of restart) (object->pointer restart))
      (restart-report restart stream)))

(defun restart-report (restart stream)
  (funcall (or (restart-report-function restart)
               (let ((name (restart-name restart)))
		 #'(lambda (stream)
		     (if name (format stream "~S" name)
			 (format stream "~S" restart)))))
           stream))

(defun find-restart (name)
  (dolist (restart-cluster *restart-clusters*)
    (dolist (restart restart-cluster)
      (when (or (eq restart name) (eq (restart-name restart) name))
	(return-from find-restart restart)))))

(defun invoke-restart (restart &rest values)
  (let ((real-restart (or (find-restart restart)
			  (error "Restart ~S is not active." restart))))
    (apply (restart-function real-restart) values)))

(defun invoke-restart-interactively (restart)
  (let ((real-restart (or (find-restart restart)
			  (error "Restart ~S is not active." restart))))
    (apply (restart-function real-restart)
	   (let ((interactive-function
		  (restart-interactive-function real-restart)))
	     (if interactive-function
		 (funcall interactive-function)
		 '())))))

(defun condition-print (condition stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<Condition ~S ~X>"
	      (type-of condition)
	      (object->pointer condition))
      (condition-report condition stream)))

(defun condition-report (condition stream)
  (do ((type (type-of condition) (parent-structure type)))
      ((not type) (format stream "The condition ~A occurred."
			  (type-of condition)))
    (let ((reporter (report-function type)))
      (when reporter
        (funcall reporter condition stream)
        (return nil)))))

(defun assert-report (names stream)
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~p for ~{~S~^, ~}."
	      (length names) names)
      (format stream ".")))

(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~
		  ~%do you want to supply a new value? "
		   name value)
	 (format *query-io* "~&Type a form to be evaluated:~%")
	 (flet ((read-it () (eval (read *query-io*))))
	   (if (symbolp name) ;help user debug lexical variables
	       (progv (list name) (list value) (read-it))
	       (read-it))))
	(t value)))

(defun simple-assertion-failure (assertion)
  (error 'simple-type-error
	 :datum assertion
	 :expected-type nil	; this needs some work in next revision. -kmp
	 :format-string "the assertion ~S failed."
	 :format-arguments (list assertion)))

(defun read-evaluated-form ()
  (format *query-io* "~&type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defun signal (datum &rest arguments)
  (let ((condition
	 (coerce-to-condition datum arguments 'simple-condition 'signal))
        (*handler-clusters* *handler-clusters*))
    (if (typep condition *break-on-signals*)
	(break "~A~%Break entered because of *BREAK-ON-SIGNALS*."
	       condition))
    (loop (if (not *handler-clusters*) (return))
          (let ((cluster (pop *handler-clusters*)))
	    (dolist (handler cluster)
	      (when (typep condition (car handler))
		(funcall (cdr handler) condition)
		(return nil)))))	;?
    nil))

;;;  Internal routine used in error, cerror, break, and warn for parsing the
;;;  hairy argument conventions into a single argument that's directly usable 
;;;  by all the other routines.
(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((typep datum 'condition)
	 (if arguments
	     (cerror "ignore the additional arguments."
		     'simple-type-error
		     :datum arguments
		     :expected-type 'null
		     :format-string "you may not supply additional arguments ~
				     when giving ~S to ~S."
		     :format-arguments (list datum function-name)))
	 datum)
        ((symbolp datum)		;roughly, (subtypep datum 'condition)
         (apply #'make-condition datum arguments))
        ((stringp datum)
	 (make-condition default-type
                         :format-string datum
                         :format-arguments arguments))
        (t (error 'simple-type-error
		  :datum datum
		  :expected-type '(or symbol string)
		  :format-string "bad argument to ~S: ~S"
		  :format-arguments (list function-name datum)))))

(defun warn (datum &rest arguments)
  (let ((condition
	 (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (check-type condition warning "a warning condition")
    (if *break-on-warnings*
	(break "~A~%break entered because of *break-on-warnings*."
	       condition))
    (restart-case (signal condition)
		  (muffle-warning ()
				  :report "skip warning."
				  (return-from warn nil)))
    (format *error-output* "~&;;; Warning: ~A~%" condition)
    nil))


(defun error (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-error 'error)))
    (signal condition)
    (format *error-output* "Error: ")
    (invoke-debugger condition)))

(defun cerror (continue-string datum &rest arguments)
  (with-simple-restart
      (continue "~A" (apply #'format nil continue-string arguments))
    (apply #'error datum arguments))
  nil)

(defun break (&optional (format-string "") &rest format-arguments)
  (with-simple-restart (continue "Return from break.")
    (format *error-output* "Break: ")
    (invoke-debugger
     (make-condition 'simple-condition
		     :format-string    format-string
		     :format-arguments format-arguments)))
  nil)

(defun show-restarts ()
  (let ((restarts (compute-restarts)))
    (loop for r in (compute-restarts)
	  for i from 0 below (length restarts)
	  do (format t "~&~D: ~A (Name: ~A) ~%" i  r (restart-name r)))))

(defun invoke-debugger (&optional (datum "Debug") &rest arguments)
  (let ((condition (coerce-to-condition
		    datum arguments 'simple-condition 'debug)))
    (when *debugger-hook*
      (let ((hook *debugger-hook*)
	    (*debugger-hook* nil))
	(funcall hook condition hook)))
    (invoke-debugger-1 condition)))

(defun invoke-debugger-1 (condition)
  (princ condition *error-output*)
  (format *error-output* "~%~%")
  (show-restarts)
  (force-output *error-output*)
  (lisp_break)
  ;; HEY! if not possible to continue from here, warn and offer restart
  ;; options.
  )

(defun gdbprint (x)
  (prog1 (prin1 x *terminal-io*)
    (force-output *terminal-io*)))

(defun gdbbacktrace (function)
  (if (atom function)
      (format *terminal-io* "~S" function)
      (format *terminal-io* "(LAMBDA ~A ...)" (second function)))
  (force-output *terminal-io*))

(defun evaldebug (init  name args venv fenv tenv benv)
  (format *terminal-io* "~%Call: ~S" (cons name args))
  (format *terminal-io* "~%Entering debug evaluator. Type :A to exit")
  (format *terminal-io*
	  "~%Evaluating forms in the current frame's lexical environment~%")
  (let ((*repl-level* (1+ *repl-level*))
	(*standard-output* *terminal-io*)
	(*standard-input* *terminal-io*))
    (repl-1 init '(:a) #'debug-prompt venv fenv tenv benv))
  nil)

(defun nullevaldebug (init)
  (format *terminal-io* "~%Entering debug evaluator. Type :A to exit")
  (format *terminal-io*
	  "~%Evaluating forms in the null lexical environment~%")
  (let ((*repl-level* (1+ *repl-level*))
	(*standard-output* *terminal-io*)
	(*standard-input* *terminal-io*))
    (repl-1 init '(:a) #'debug-prompt))
  nil)

(defun select-restart-option ()
  (let ((restarts (compute-restarts)))
    (show-restarts)
    (let ((option (prompt-for `(integer 0 ,(length restarts))
			      "Restart Number: ")))
      (invoke-restart-interactively (elt restarts option)))))

(defun prompt-for (type &optional prompt)
  (flet ((try () (format t "~&~A" (or prompt type)) (read)))
    (do ((ans (try) (try)))
        ((typep ans type) ans)
      (format t "~&Invalid option, value must be of type:  ~S~%" type))))

(defun prompt-for-value (stream)
  (list (eval (prompt-for t "Value: "))))

;;; HEY! use a restart for this...
(defun abort-to-top-level ()
  (throw 'top-level nil))

(defun abort ()
  (invoke-restart 'abort)
  (error 'abort-failure))

(defun continue  ()
  (invoke-restart 'continue))

(defun muffle-warning ()
  (invoke-restart 'muffle-warning))

(defun store-value (value)
  (invoke-restart 'store-value value))

(defun use-value (value)
  (invoke-restart 'use-value   value))

(defun symbol-function-error (symbol)
  (restart-case
   (error 'undefined-function :name symbol)
   (use-value (value)
	      :report (lambda (stream)
			(format stream
				"Specify a function to use instead of ~S"
				symbol))
	      :interactive prompt-for-value
	      value)
   (nil ()
	:report (lambda (stream)
		  (format stream
			  "Retry symbol-function of ~S."
			  symbol))
	(symbol-function symbol))
   (store-value (value)
		:report (lambda (stream)
			  (format stream
				  "Specify a new functions value for ~A"
				  symbol))
		:interactive prompt-for-value
		(setf (symbol-function symbol) value)
		value)))

#| Sample usage 
;;;
;;; to install this condition system, you must make your evaluator call the
;;; functions above. to make it more useful,
;;; you should try to establish reasonable
;;; restarts. what follows is an illustration of how this might be done if your
;;; evaluator were actually written in lisp. (note: the evaluator shown is not
;;; following cl evaluation rules, but that's not relevant to the points
;;; we're trying to make here.)

(define-debug-command :abort ()
  (if *debug-abort*
      (invoke-restart-interactively *debug-abort*)
      (format t "~&there is no way to abort.~%")))

(define-debug-command :continue ()
  (if *debug-continue*
      (invoke-restart-interactively *debug-continue*)
      (format t "~&there is no way to continue.~%")))

(define-debug-command :error ()
  (format t "~&~A~%" *debug-condition*))

(defun my-prompt-for-value ()
  (list (my-eval (prompt-for t "value"))))

(defun my-repl ()
  (let ((*debug-eval* 'my-eval)
	(*debug-print* 'my-print))
    (do ((form (prompt-for 't "eval")
               (prompt-for 't "eval")))
        ((not form))
      (with-simple-restart (abort "return to my-repl toplevel.")
	(my-print (multiple-value-list (my-eval form)))))))

(defun my-print (values)
  (format t "~{~&=> ~s~}" values))

(defun my-apply (fn &rest args)
  (if (functionp fn)
      (apply #'apply fn args)
      (restart-case (error "invalid function: ~s" fn)
		    (use-value (x)
			       :report "use a different function."
			       :interactive my-prompt-for-value
			       (apply #'my-apply x args)))))

|#
