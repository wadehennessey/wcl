;;; The following is taken from Cltl2 and modified
;;; just a little bit. I haven't typed all the comments in.
;;; Refer to Appendix C of the Cltl2.
;;;
;;; Common Lisp backquote implementation, written in Common Lisp 
;;; Author: Guy L. Steele Jr.   Date: 27 December 1985
;;; This software is in the public domain

;;; The following are unique tokens used during processing
;;; They need not be symbols; they need not even be atoms.

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens (if *bq-simplify*
			  (bq-simplify raw-result)
			  raw-result))))

(defun bq-process (x)
  (cond ((atom x)
	 (list *bq-quote* x))
	((eq (car x) 'backquote)
	 (bq-process (bq-completely-process (cadr x))))
	((eq (car x) *comma*) (cadr x))
	((eq (car x) *comma-atsign*)
	 (error ",@~S after `" (cadr x)))
	((eq (car x) *comma-dot*)
	 (error ",. ~S after `" (cadr x)))
	(t (do ((p x (cdr p))
		(q '() (cons (bracket (car p)) q)))
	       ((atom p)
		(cons *bq-append*
		      (nreconc q (list (list *bq-quote* p)))))
	     (when (eq (car p) *comma*)
	       (unless (null (cddr p)) (error "Malformed ,~S" p))
	       (return (cons *bq-append*
			     (nreconc q (list (cadr p))))))
	     (when (eq (car p) *comma-atsign*)
	       (error "Dotted ,@~S" p))
	     (when (eq (car p) *comma-dot*)
	       (error "Dotted ,.~S" p))))))

(defun bracket (x)
  (cond ((atom x)
	 (list *bq-list* (bq-process x)))
	((eq (car x) *comma*)
	 (list *bq-list* (cadr x)))
	((eq (car x) *comma-atsign*)
	 (cadr x))
	((eq (car x) *comma-dot*)
	 (list *bq-clobberable* (cadr x)))
	(t (list *bq-list* (bq-process x)))))

(defun maptree (fn x)
  (if (atom x)
      (funcall fn x)
      (let ((a (funcall fn (car x)))
	    (d (maptree fn (cdr x))))
	(if (and (eql a (car x)) (eql d (cdr x)))
	    x
	    (cons a d)))))

(defun bq-splicing-frob (x)
  (and (consp x)
       (or (eq (car x) *comma-atsign*)
	   (eq (car x) *comma-dot*))))

(defun bq-frob (x)
  (and (consp x)
       (or (eq (car x) *comma*)
	   (eq (car x) *comma-atsign*)
	   (eq (car x) *comma-dot*))))

(defun bq-simplify (x)
  (if (atom x)
      x
      (let ((x (if (eq (car x) *bq-quote*)
		   x
		   (maptree #'bq-simplify x))))
	(if (not (eq (car x) *bq-append*))
	    x
	    (bq-simplify-args x)))))

(defun bq-simplify-args (x)
  (let ((r (reverse (cdr x))))
    (do ((args (cdr r) (cdr args))
	 (result
	  (car r)
	  (cond ((atom (car args))
		 (bq-attach-append *bq-append* (car args) result))
		((and (eq (caar args) *bq-list*)
		      (notany #'bq-splicing-frob (cdar args)))
		 (bq-attach-conses (cdar args) result))
		((and (eq (caar args) *bq-list**)
		      (notany #'bq-splicing-frob (cdar args)))
		 (bq-attach-conses
		  (reverse (cdr (reverse (cdar args))))
		  (bq-attach-append *bq-append*
				    (car (last (car args)))
				    result)))
		((and (eq (caar args) *bq-quote*)
		      (consp (cadar args))
		      (not (bq-frob (cadar args)))
		      (null (cddar args)))
		 (bq-attach-conses (list (list *bq-quote*
					       (caadar args)))
				   result))
		((eq (caar args) *bq-clobberable*)
		 (bq-attach-append *bq-nconc* (cadar args) result))
		(t (bq-attach-append *bq-append*
				     (car args)
				     result)))))
	((null args) result))))

(defun null-or-quoted (x)
  (or (null x) (and (consp x) (eq (car x) *bq-quote*))))

(defun bq-attach-append (op item result)
  (cond ((and (null-or-quoted item) (null-or-quoted result))
	 (list *bq-quote* (append (cadr item) (cadr result))))
	((or (null result) (equal result *bq-quote-nil*))
	 (if (bq-splicing-frob item) (list op item) item))
	((and (consp result) (eq (car result) op))
	 (list* (car result) item (cdr result)))
	(t (list op item result))))

(defun bq-attach-conses (items result)
  (cond ((and (every #'null-or-quoted items)
	      (null-or-quoted result))
	 (list *bq-quote*
	       (append (mapcar #'cadr items) (cadr result))))
	((or (null result) (equal result *bq-quote-nil*))
	 (cons *bq-list* items))
	((and (consp result)
	      (or (eq (car result) *bq-list*)
		  (eq (car result) *bq-list**)))
	 (cons (car result) (append items (cdr result))))
	(t (cons *bq-list** (append items (list result))))))

(defun bq-remove-tokens (x)
  (cond ((eq x *bq-list*) 'list)
	((eq x *bq-append*) 'append)
	((eq x *bq-nconc*) 'nconc)
	((eq x *bq-list**) 'list*)
	((eq x *bq-quote*) 'quote)
	((atom x) x)
	((eq (car x) *bq-clobberable*)
	 (bq-remove-tokens (cadr x)))
	((and (eq (car x) *bq-list**)
	      (consp (cddr x))
	      (null (cdddr x)))
	 (cons 'cons (maptree #'bq-remove-tokens (cdr x))))
	(t (maptree #'bq-remove-tokens x))))


(defun char-macro-read-backquote (stream char)
  (declare (ignore char))
  (let ((*bq-count* (1+ *bq-count*)))
    (list 'backquote (read stream t nil t))))


(defun char-macro-read-comma (stream char)
  (declare (ignore char))
  (if (> *bq-count* 0)
      (case (peek-char nil stream t nil t)
	(#\@ (read-char stream t nil t)
	     (list *comma-atsign* (read stream t nil t)))
	(#\. (read-char stream t nil t)
	     (list *comma-dot* (read stream t nil t)))
	(otherwise (list *comma* (read stream t nil t))))
      (error "A comma was read without a matching backquote")))
  
