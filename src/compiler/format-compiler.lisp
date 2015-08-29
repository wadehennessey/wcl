;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; HEY! This stuff isn't used. Maybe we'll just use the much
;;; more complete (though less efficient) format compiler in
;;; the pprint stuff.

;;; A simple format compiler. Used as a source-level optimizer.

;(define-compiler-macro format (&whole form stream string &rest args)
;  (compile-format form stream str args))

;;; Things to add:
;;;    LET bind format args if they are to be used more than once
;;;    prefix args
(defun compile-format (whole stream-form str args)
  (let ((stream (if (or (not (atom stream-form)) (null stream-form))
		    (gensym "STREAM")
		    stream-form)))
    (flet ((write-const-string (start end)
	     (if (= start end)
		 nil
		 `((write-string ,(subseq str start end) ,stream)))))
      (iterate munch ((start 0)
		      (end 0)
		      (argi 0)
		      (out nil))
	(cond ((>= end (length str))
	       (let ((body (reverse (append (write-const-string
					     start end)
					    out))))
		 (if (eq stream-form stream)
		     `(progn ,@body nil)
		     (if (null stream-form)
			 `(with-output-to-string (,stream)
			   ,@body)
			 `(let ((,stream ,stream-form))
			   ,@body
			   nil)))))
	      ((char= (aref str end) #\~)
	       ;; Ignore prefix args. Assume cmd char next
	       (let* ((const-str (write-const-string start end))
		      (next-arg (if (< argi (length args))
				    (elt args argi)
				    nil))
		      (form (case (char-upcase (aref str (1+ end)))
			      (#\A `(princ ,next-arg ,stream))
			      (#\S `(prin1 ,next-arg ,stream))
			      (#\C `(write-char ,next-arg ,stream))
			      (#\% `(terpri ,stream))
			      (#\X `(write ,next-arg :base 16 :stream ,stream))
			      (#\D `(write ,next-arg :base 10 :stream ,stream))
			      (#\O `(write ,next-arg :base 8 :stream ,stream))
			      (#\B `(write ,next-arg :base 2 :stream ,stream))
			      ;; This case could be more efficient.
			      (#\~ `(write-string "~" ,stream))
			      (t :abort)))) ; punt on cmds we don't know
		 (case form
		   (:abort whole)
		   (t (munch (+ end 2)
			     (+ end 2)
			     (1+ argi)
			     (cons form (append const-str out)))))))
	      (t (munch start (1+ end) argi out)))))))


