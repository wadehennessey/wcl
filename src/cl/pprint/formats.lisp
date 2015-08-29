;;; Copyright 1989,1990 by the Massachusetts Institute of Technology,
;;; Cambridge, Massachusetts.

(in-package "XP")

;;;                 ---- PRETTY PRINTING FORMATS ----

(defun pretty-array (xp array)
  (cond ((vectorp array) (pretty-vector xp array))
	((zerop (array-rank array))
	 (write-string++ "#0A " xp 0 4)
	 (write+ (aref array) xp))
	(T (pretty-non-vector xp array))))

(defun pretty-vector (xp v)
  (pprint-logical-block (xp nil :prefix "#(" :suffix ")")
    (let ((end (length v)) (i 0))
      (when (plusp end)
	(loop (pprint-pop)
	      (write+ (aref v i) xp)
	      (if (= (incf i) end) (return nil))
	      (write-char++ #\space xp)
	      (pprint-newline+ :fill xp))))))

(defun pretty-non-vector (xp array)
  (let* ((bottom (1- (array-rank array)))
	 (indices (make-list (1+ bottom) :initial-element 0))
	 (dims (array-dimensions array)))
    (funcall (formatter "#~DA") xp (1+ bottom))
    (labels ((pretty-slice (slice)
	       (pprint-logical-block (xp nil :prefix "(" :suffix ")")
		 (let ((end (nth slice dims))
		       (spot (nthcdr slice indices))
		       (i 0))
		   (when (plusp end)
		     (loop (pprint-pop)
			   (setf (car spot) i)
			   (if (= slice bottom)
			       (write+ (apply #'aref array indices) xp)
			       (pretty-slice (1+ slice)))
			   (if (= (incf i) end) (return nil))
			   (write-char++ #\space xp)
			   (pprint-newline+ (if (= slice bottom) :fill :linear) xp)))))))
      (pretty-slice 0))))

;;; Must use pprint-logical-block (no +) in the following three,
;;; because they are exported functions.
(defun pprint-linear (s list &optional (colon? T) atsign?)
     (declare (ignore atsign?))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-newline+ :linear s))))

(defun pprint-fill (s list &optional (colon? T) atsign?)
    (declare (ignore atsign?))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-newline+ :fill s))))

(defun pprint-tabular (s list &optional (colon? T) atsign? (tabsize nil))
  (declare (ignore atsign?))
  (when (null tabsize) (setq tabsize 16))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-tab+ :section-relative 0 tabsize s)
	  (pprint-newline+ :fill s))))

(defun fn-call (xp list)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list))

;;; Although idiosyncratic, I have found this very useful to avoid large
;;; indentations when printing out code.

(defun alternative-fn-call (xp list)
  (if (> (length (symbol-name (car list))) 12)
      (funcall (formatter "~:<~1I~@{~W~^ ~_~}~:>") xp list)
      (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list)))

(defun bind-list (xp list &rest args)
    (declare (ignore args))
  (if (do ((i 50 (1- i))
	   (ls list (cdr ls))) ((null ls) t)
	(when (or (not (consp ls)) (not (symbolp (car ls))) (minusp i))
	  (return nil)))
      (pprint-fill xp list)
      (funcall (formatter "~:<~@{~:/xp:pprint-fill/~^ ~_~}~:>") xp list)))

(defun block-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~^~W~^ ~@_~W~^~@{ ~_~W~^~}~:>") xp list))

(defun defun-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~W~^ ~@_~W~^ ~@_~:/xp:pprint-fill/~^~@{ ~_~W~^~}~:>")
	   xp list))

(defun print-fancy-fn-call (xp list template)
  (let ((i 0) (in-first-section T))
    (pprint-logical-block+ (xp list "(" ")" nil T nil)
      (write+ (pprint-pop) xp)
      (pprint-indent+ :current 1 xp)
      (loop
	(pprint-exit-if-list-exhausted)
	(write-char++ #\space xp)
	(when (eq i (car template))
	  (pprint-indent+ :block (cadr template) xp)
	  (setq template (cddr template))
	  (setq in-first-section nil))
	(pprint-newline (cond ((and (zerop i) in-first-section) :miser)
			      (in-first-section :fill)
			      (T :linear))
			xp)
	(write+ (pprint-pop) xp)
	(incf i)))))

(defun maybelab (xp item &rest args)
    (declare (ignore args) (special need-newline indentation))
  (when need-newline (pprint-newline+ :mandatory xp))
  (cond ((and item (symbolp item))
	 (write+ item xp)
	 (setq need-newline nil))
	(T (pprint-tab+ :section indentation 0 xp)
	   (write+ item xp)
	   (setq need-newline T))))

(defun function-call-p (x)
  (and (consp x) (symbolp (car x)) (fboundp (car x))))


;;; THE FOLLOWING STUFF SETS UP THE DEFAULT *PRINT-PPRINT-DISPATCH*
;;; 
;;; This is an attempt to specify a correct format for every form
;;; in the CL book
;;; that does not just get printed out like an ordinary function call 
;;; (i.e., most special forms and many macros).  This of course does not 
;;; cover anything new you define.

(defun let-print (xp obj)
  (funcall
   (formatter "~:<~1I~W~^ ~@_~/xp:bind-list/~^~@{ ~_~W~^~}~:>") xp obj))

(defun cond-print (xp obj)
  (funcall
   (formatter "~:<~W~^ ~:I~@_~@{~:/xp:pprint-linear/~^ ~_~}~:>") xp obj))

(defun dmm-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun defsetf-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun do-print (xp obj)
  (funcall 
 (formatter "~:<~W~^ ~:I~@_~/xp:bind-list/~^ ~_~:/xp:pprint-linear/ ~1I~^~@{ ~_~W~^~}~:>")
           xp obj))


(defun flet-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~:<~@{~/xp:block-like/~^ ~_~}~:>~^~@{ ~_~W~^~}~:>")
	   xp obj))

(defun function-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "#'~W") xp (cadr list))
      (fn-call xp list)))

(defun mvb-print (xp list)
  (print-fancy-fn-call xp list '(1 3 2 1)))

(defun prog-print (xp list)
  (let ((need-newline T) (indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (formatter "~:<~W~^ ~:/xp:pprint-fill/~^ ~@{~/xp:maybelab/~^ ~}~:>")
	     xp list)))

(defun setq-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~:_~W~^ ~_~}~:>") xp obj))

(defun quote-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "'~W") xp (cadr list))
      (pprint-fill xp list)))

(defun tagbody-print (xp list)
  (let ((need-newline (and (consp (cdr list))
			   (symbolp (cadr list)) (cadr list)))
	(indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (formatter "~:<~W~^ ~@{~/xp:maybelab/~^ ~}~:>") xp list)))

(defun up-print (xp list)
  (print-fancy-fn-call xp list '(0 3 1 1)))

;;; here is some simple stuff for printing LOOP
;;;
;;; The challange here is that we have to effectively parse the clauses of the
;;; loop in order to know how to print things.  Also you want to do this in a 
;;; purely incremental way so that all of the abbreviation things work, and
;;; you wont blow up on circular lists or the like.  (More aesthic output could
;;; be produced by really parsing the clauses into nested lists before
;;; printing them.)

;;; The following program assumes the following simplified grammar of the loop
;;; clauses that explains how to print them.  Note that it does not bare much
;;; resemblence to the right parsing grammar, however, it produces half decent
;;; output.  The way to make the output better is to make the grammar more
;;; detailed.  
;;; 
;;; loop == (LOOP {clause}*)      ;one clause on each line.
;;; clause == block | linear | cond | finally
;;; block == block-head {expr}*   ;as many exprs as possible on each line.
;;; linear == linear-head {expr}* ;one expr on each line.
;;; finally == FINALLY [DO | DOING | RETURN] {expr}* ;one expr on each line.
;;; cond == cond-head [expr]
;;;           clause
;;; 	   {AND clause}*       ;one AND on each line.
;;;         [ELSE
;;;           clause
;;; 	   {AND clause}*]      ;one AND on each line.
;;;         [END]
;;; block-head == FOR | AS | WITH | AND
;;;      | REPEAT | NAMED | WHILE | UNTIL | ALWAYS | NEVER | THEREIS | RETURN
;;;      | COLLECT | COLLECTING | APPEND | APPENDING | NCONC | NCONCING | COUNT
;;;      | COUNTING | SUM | SUMMING | MAXIMIZE | MAXIMIZING | MINIMIZE 
;;;      |  MINIMIZING 
;;; linear-head == DO | DOING | INITIALLY
;;; var-head == FOR | AS | WITH
;;; cond-head == IF | WHEN | UNLESS
;;; expr == <anything that is not a head symbol>

;;; Note all the string comparisons below are required to support some
;;; existing implementations of LOOP.

(defun token-type (token &aux string)
  (cond ((not (symbolp token)) :expr)
	((string= (setq string (string token)) "FINALLY") :finally)
	((member string '("IF" "WHEN" "UNLESS") :test #'string=) :cond-head)
	((member string '("DO" "DOING" "INITIALLY") :test #'string=) :linear-head)
	((member string '("FOR" "AS" "WITH" "AND" "END" "ELSE"
			  "REPEAT" "NAMED" "WHILE" "UNTIL" "ALWAYS" "NEVER"
			  "THEREIS" "RETURN" "COLLECT" "COLLECTING" "APPEND"
			  "APPENDING" "NCONC" "NCONCING" "COUNT" "COUNTING"
			  "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
			  "MINIMIZE" "MINIMIZING")
		 :test #'string=)
	 :block-head)
	(T :expr)))

(defun pretty-loop (xp loop)
  (if (not (and (consp (cdr loop)) (symbolp (cadr loop)))) ; old-style loop
      (function-print xp loop)
      (pprint-logical-block (xp loop :prefix "(" :suffix ")")
	(let (token type)
	  (labels ((next-token ()
		     (pprint-exit-if-list-exhausted)
		     (setq token (pprint-pop))
		     (setq type (token-type token)))
		   (print-clause (xp)
		     (case type
		       (:linear-head (print-exprs xp nil :mandatory))
		       (:cond-head (print-cond xp))
		       (:finally (print-exprs xp T :mandatory))
		       (otherwise (print-exprs xp nil :fill))))
		   (print-exprs (xp skip-first-non-expr newline-type)
		     (let ((first token))
		       (next-token)	;so always happens no matter what
		       (pprint-logical-block (xp nil)
			 (write first :stream xp)
			 (when (and skip-first-non-expr (not (eq type :expr)))
			   (write-char #\space xp)
			   (write token :stream xp)
			   (next-token))
			 (when (eq type :expr)
			   (write-char #\space xp)
			   (pprint-indent :current 0 xp)
			   (loop (write token :stream xp)
				 (next-token)
				 (when (not (eq type :expr)) (return nil))
				 (write-char #\space xp)
				 (pprint-newline newline-type xp))))))
		   (print-cond (xp)
		     (let ((first token))
		       (next-token)	;so always happens no matter what
		       (pprint-logical-block (xp nil)
			 (write first :stream xp)
			 (when (eq type :expr)
			   (write-char #\space xp)
			   (write token :stream xp)
			   (next-token))
			 (write-char #\space xp)
			 (pprint-indent :block 2 xp)
			 (pprint-newline :linear xp)
			 (print-clause xp)
			 (print-and-list xp)
			 (when (string= (string token) "ELSE")
			   (print-else-or-end xp)
			   (write-char #\space xp)
			   (pprint-newline :linear xp)
			   (print-clause xp)
			   (print-and-list xp))
			 (when (string= (string token) "END")
			   (print-else-or-end xp)))))
		   (print-and-list (xp)
		     (loop (when (not (string= (string token) "AND")) (return nil))
			   (write-char #\space xp)
			   (pprint-newline :mandatory xp)
			   (write token :stream xp)
			   (next-token)
			   (write-char #\space xp)
			   (print-clause xp)))
		   (print-else-or-end (xp)
		     (write-char #\space xp)
		     (pprint-indent :block 0 xp)
		     (pprint-newline :linear xp)
		     (write token :stream xp)
		     (next-token)
		     (pprint-indent :block 2 xp)))
	    (pprint-exit-if-list-exhausted)
	    (write (pprint-pop) :stream xp)
	    (next-token)
	    (write-char #\space xp)
	    (pprint-indent :current 0 xp)
	    (loop (print-clause xp)
		  (write-char #\space xp)
		  (pprint-newline :linear xp)))))))

;;; WCL BQ stuff
(defun backquote-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "`~W") xp (cadr list))
      (fn-call xp list)))

(defun comma-print (xp list)
  (funcall (formatter ",~W") xp (cadr list)))

(defun comma-atsign-print (xp list)
  (funcall (formatter ",@~W") xp (cadr list)))

(defun comma-dot-print (xp list)
  (funcall (formatter ",.~W") xp (cadr list)))
