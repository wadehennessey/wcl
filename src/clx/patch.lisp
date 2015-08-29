
(in-package "XLIB")

;;; A bug in GCC 2.1 prevents using -O when compiling input.lisp.
;;; [in READ-INPUT nil assign from SHIFTF is ignored, events are lost]
;;; Only use sun cc to compile, or manually compile input.c without -O.

(defun read-input (display timeout force-output-p predicate &rest predicate-args)
  (let ((reply-buffer nil)
	(token (or (current-process) (cons nil nil))))
    (unwind-protect 
	(tagbody
	 loop
	 (when (display-dead display)
	   (error 'closed-display :display display))
	 (when (apply predicate predicate-args)
	   (return-from read-input nil))
	 ;; Check and see if we have to force output
	 (when (and force-output-p
		    (or (and (not (eq (display-input-in-progress display) token))
			     (not (conditional-store
				   (display-input-in-progress display) nil token)))
			(null (buffer-listen display))))
	   (go force-output))
	 ;; Ensure that ony one process is reading input.
	 (unless (or (eq (display-input-in-progress display) token)
		     (conditional-store (display-input-in-progress display) nil token))
	   (if (eql timeout 0)
	       (return-from read-input :timeout)
	     (apply #'process-block "CLX Input Lock"
		    #'(lambda (display predicate &rest predicate-args)

			(or (apply predicate predicate-args)
			    (null (display-input-in-progress display))
			    (not (null (display-dead display)))))
		    display predicate predicate-args))
	   (go loop))
	 ;; Now start gobbling.
	 (setq reply-buffer (allocate-event))
	 (with-buffer-input (reply-buffer :sizes (8 16 32))
			    (let ((type 0))
			      ;; Wait for input before we disallow aborts.
			      (unless (eql timeout 0)
				(let ((eof-p (buffer-input-wait display timeout)))
				  (when eof-p (return-from read-input eof-p))))
			      (without-aborts
			       (let ((eof-p (buffer-input display buffer-bbuf 0 *replysize*
							  (if force-output-p 0 timeout))))
				 (when eof-p
				   (when (eq eof-p :timeout)
				     (if force-output-p
					 (go force-output)
				       (return-from read-input :timeout)))
				   (setf (display-dead display) t)
				   (return-from read-input eof-p)))
			       (setf (reply-data-size reply-buffer) *replysize*)
			       (when (= (the card8 (setq type (read-card8 0))) 1)
				 ;; Normal replies can be longer than *replysize*, so we
				 ;; have to handle them while aborts are still disallowed.
				 (let ((value
					(read-reply-input
					 display (read-card16 2)
					 (index+ *replysize* (index* (read-card32 4) 4))
					 (shiftf reply-buffer nil))))
				   (when value
				     (return-from read-input value))
				   (go loop))))
			      (if (zerop type)
				  (read-error-input
				   display (read-card16 2) (shiftf reply-buffer nil) token)
				(read-event-input
				 display (read-card8 0) (shiftf reply-buffer nil)))))
	 (go loop)
	 force-output 
	 (note-input-complete display token)
	 (display-force-output display)
	 (setq force-output-p nil)
	 (go loop))
      (when (not (null reply-buffer))
	(deallocate-reply-buffer reply-buffer))
      (note-input-complete display token))))