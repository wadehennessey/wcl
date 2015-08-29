;;; Copyright 1989,1990 by the Massachusetts Institute of Technology,
;;; Cambridge, Massachusetts.

(in-package "XP")

;;;	   ---- BASIC INTERFACE FUNCTIONS ----
;;;
;;; The internal functions in this file, and the (formatter "...") expansions
;;; use the '+' forms of these functions directly (which is faster) because,
;;; they do not need error checking of fancy stream coercion.  The '++' forms
;;; additionally assume the thing being output does not contain a newline.

(defun write (object &rest pairs &key (stream *standard-output*)
		     (escape *print-escape*) (radix *print-radix*)
		     (base *print-base*) (circle *print-circle*)
		     (pretty *print-pretty*) (level *print-level*)
		     (length *print-length*) (case *print-case*)
		     (gensym *print-gensym*) (array *print-array*)
		     (pprint-dispatch *print-pprint-dispatch*)
		     (right-margin *print-right-margin*)
		     (lines *print-lines*) (miser-width *print-miser-width*))
  (setq stream (decode-stream-arg stream))
  (let ((*print-pprint-dispatch* pprint-dispatch) (*print-right-margin* right-margin)
	(*print-lines* lines) (*print-miser-width* miser-width))
    (cond ((or (xp-structure-p stream) pretty)
	   (let ((*print-escape* escape) (*print-radix* radix)
		 (*print-base* base) (*print-circle* circle)
		 (*print-pretty* pretty) (*print-level* level)
		 (*print-length* length) (*print-case* case)
		 (*print-gensym* gensym) (*print-array* array))
	     (basic-write object stream)))
	  (T (remf pairs :dispatch) (remf pairs :right-margin)
	     (remf pairs :lines) (remf pairs :miser-width)
	     (apply #'lisp:write object pairs))))
  object)

(defun basic-write (object stream)
  (cond ((xp-structure-p stream) (write+ object stream))
	(*print-pretty* (maybe-initiate-xp-printing
			 #'(lambda (s o) (write+ o s)) stream object))
	(T (lisp:write object :stream stream))))

(defun maybe-initiate-xp-printing (fn stream &rest args)
  (if (xp-structure-p stream) (apply fn stream args)
      (let ((*abbreviation-happened* nil)
	    (*locating-circularities* (if *print-circle* 0 nil))
	    (*circularity-hash-table*
	     (if *print-circle* (get-circularity-hash-table) nil))
	    (*parents* (when (not *print-shared*) (list nil)))
	    (*result* nil))
	(xp-print fn (decode-stream-arg stream) args)
	(if *circularity-hash-table*
	    (free-circularity-hash-table *circularity-hash-table*))
	(when *abbreviation-happened* 
	  (setq *last-abbreviated-printing*
		(eval
		 `(function
		   (lambda (&optional (stream ',stream))
		    (let ((*package* ',*package*))
		      (apply #'maybe-initiate-xp-printing
			     ',fn stream
			     ',(copy-list args))))))))
	*result*)))

(defun xp-print (fn stream args)
  (setq *result* (do-xp-printing fn stream args))
  (when *locating-circularities*
    (setq *locating-circularities* nil)
    (setq *abbreviation-happened* nil)
    (setq *parents* nil)
    (setq *result* (do-xp-printing fn stream args))))

;;; HEY! This is with-valid-stream in WCL
(defun decode-stream-arg (stream)
  (cond ((eq stream T) *terminal-io*)
	((null stream) *standard-output*)
	(T stream)))

(defun do-xp-printing (fn stream args)
  (let ((xp (get-pretty-print-stream stream))
	(*current-level* 0)
	(result nil))
    (catch 'line-limit-abbreviation-exit
      (start-block xp nil nil nil)
      (setq result (apply fn xp args))
      (end-block xp nil))
    (when (and *locating-circularities*
	       (zerop *locating-circularities*)	;No circularities.
	       (= (line-no xp) 1)	     	;Didn't suppress line.
	       (zerop (buffer-offset xp)))	;Didn't suppress partial line.
      (setq *locating-circularities* nil))	;print what you have got.
    (when (catch 'line-limit-abbreviation-exit
	    (attempt-to-output xp nil T) nil)
      (attempt-to-output xp T T))
    (free-pretty-print-stream xp)
    result))

(defun write+ (object xp)
  (let ((*parents* *parents*))
    (unless (and *circularity-hash-table*
		 (eq (circularity-process xp object nil) :subsequent))
      (when (and *circularity-hash-table* (consp object))
	;; avoid possible double check in handle-logical-block.
	(setq object (cons (car object) (cdr object))))
      (let ((printer (if *print-pretty* (get-printer object *print-pprint-dispatch*) nil))
	    type)
	(cond (printer (funcall printer xp object))
	      ((maybe-print-fast xp object))
	      ((and *print-pretty*
		    (symbolp (setq type (type-of object)))
		    (setq printer (get type 'structure-printer))
		    (not (eq printer :none)))
	       (funcall printer xp object))
	      ((and *print-pretty* *print-array* (arrayp object)
		    (not (stringp object)) (not (bit-vector-p object))
		    (not (structure-type-p (type-of object))))
	       (pretty-array xp object))
	      (T (let ((stuff
			(with-output-to-string (s)
			  (non-pretty-print object s))))
		   (write-string+ stuff xp 0 (length stuff)))))))))

(defun non-pretty-print (object s)
  (lisp:write object
	      :level (if *print-level*
			 (- *print-level* *current-level*))
	      :pretty nil
	      :stream s))

;;; It is vital that this function be called EXACTLY once for each
;;; occurrence of  each thing in something being printed.
;;; Returns nil if printing should just continue on.
;;; Either it is not a duplicate, or we are in the first pass and do not know.
;;; returns :FIRST if object is first occurrence of a DUPLICATE.
;;;  (This can only be returned on a second pass.)
;;;  After an initial code (printed by this routine on the second pass)
;;;  printing should continue on for the object.
;;; returns :SUBSEQUENT if second or later occurrence.
;;;  Printing is all taken care of by this routine.
;;; Note many (maybe most) lisp implementations have characters
;;; and small numbers represented in a single word so that the are
;;; always eq when they are equal and the
;;; reader takes care of properly sharing them (just as it does with symbols).
;;; Therefore, we do not want circularity processing applied to them.  However,
;;; some kinds of numbers (e.g., bignums) undoubtedly are complex structures
;;; that the reader does not share.  However, they cannot have circular
;;; pointers in them and it is therefore probably a waste to do circularity
;;; checking on them.  In any case, it is not clear that it easy to
;;; tell exactly what kinds of numbers a given implementation of CL is going
;;; to have the reader automatically share.
(defun circularity-process (xp object interior-cdr?)
  (unless (or (numberp object)
	      (characterp object)
	      (and (symbolp object)	;Reader takes care of sharing.
		   (or (null *print-gensym*) (symbol-package object)))) 
    (let ((id (gethash object *circularity-hash-table*)))
      (if *locating-circularities*
	  (cond ((null id)	;never seen before
		 (when *parents* (push object *parents*))
		 (setf (gethash object *circularity-hash-table*) 0)
		 nil)
		((zerop id) ;possible second occurrence
		 (cond ((or (null *parents*) (member object *parents*))
			(setf (gethash object *circularity-hash-table*)
			      (incf *locating-circularities*))
			:subsequent)
		       (T nil)))
		(T :subsequent));third or later occurrence
	  (cond ((or (null id)	;never seen before (note ~@* etc. conses)
		     (zerop id));no duplicates
		 nil)
		((plusp id)
		 (cond (interior-cdr?
			(decf *current-level*)
			(write-string++ ". #" xp 0 3))
		       (T (write-char++ #\# xp)))
		 (print-fixnum xp id)
		 (write-char++ #\= xp)
		 (setf (gethash object *circularity-hash-table*) (- id))
		 :first)
		(T (if interior-cdr? (write-string++ ". #" xp 0 3)
		       (write-char++ #\# xp))
		   (print-fixnum xp (- id))
		   (write-char++ #\# xp)
		   :subsequent))))))

;;; This prints a few very common, simple atoms very fast.
;;; Pragmatically, this turns out to be an enormous savings over going to the
;;; standard printer all the time.  There would be diminishing
;;; returns from making this work with more things, but might be worth it.
(defun maybe-print-fast (xp object)
  (cond ((stringp object)
	 (cond ((null *print-escape*)
		(write-string+ object xp 0 (length object)) T)
	       ((every #'(lambda (c) (not (or (char= c #\") (char= c #\\))))
		       object)
		(write-char++ #\" xp)
		(write-string+ object xp 0 (length object))
		(write-char++ #\" xp) T)))
	((typep object 'fixnum)
	 (when (and (null *print-radix*) (= *print-base* 10.))
	   (when (minusp object)
	     (write-char++ #\- xp)
	     (setq object (- object)))
	   (print-fixnum xp object) T))
	((symbolp object)
	 (let ((s (symbol-name object))
	       (p (symbol-package object))
	       (is-key (keywordp object))
	       (mode (case *print-case*
		       (:downcase :down)
		       (:capitalize :cap1)
		       ;;note no-escapes-needed requires all caps
		       (T nil))))	
	   (cond ((and (or is-key (eq p *package*)
			   (and *package* ;can be NIL on symbolics
				(eq object (find-symbol s))))
		       (no-escapes-needed s))
		  (when (and is-key *print-escape*)
		    (write-char++ #\: xp))
		  (if mode (push-char-mode xp mode))
		  (write-string++ s xp 0 (length s))
		  (if mode (pop-char-mode xp)) T))))))

(defun print-fixnum (xp fixnum)
  (multiple-value-bind (digits d)
      (truncate fixnum 10)
    (unless (zerop digits)
      (print-fixnum xp digits))
    (write-char++ (code-char (+ #.(char-code #\0) d)) xp)))

;;; just wants to succeed fast in a lot of common cases.
;;; assumes no funny readtable junk for the characters shown.
(defun no-escapes-needed (s)
  (let ((n (length s)))
    (and (not (zerop n))
	 (let ((c (schar s 0)))
	   (or (and (alpha-char-p c) (upper-case-p c)) (find c "*<>")))
	 (do ((i 1 (1+ i))) ((= i n) T)
	   (let ((c (schar s i)))
	     (if (not (or (digit-char-p c)
			  (and (alpha-char-p c) (upper-case-p c))
			  (find c "*+<>-")))
		 (return nil)))))))


(defun print (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (terpri stream)
  (let ((*print-escape* T))
    (basic-write object stream))
  (write-char #\space stream)
  object)

(defun prin1 (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (let ((*print-escape* T))
    (basic-write object stream))
  object)

(defun princ (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (let ((*print-escape* nil))
    (basic-write object stream))
  object)

(defun pprint (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (terpri stream)
  (let ((*print-escape* T) (*print-pretty* T))
    (basic-write object stream))
  (values))

(defun write-to-string (object &rest pairs &key &allow-other-keys)
  (with-output-to-string (s)
    (apply #'write object :stream s pairs)))

(defun prin1-to-string (object)
  (with-output-to-string (stream)
    (let ((*print-escape* T))
      (basic-write object stream))))

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (let ((*print-escape* nil))
      (basic-write object stream))))

;;; Any format string that is converted to a function is always printed
;;; via an XP stream (See formatter).
(defun format (stream string-or-fn &rest args)
  (cond ((stringp stream)
	 (lisp:format stream "~A"
		      (with-output-to-string (stream)
			(apply #'format stream string-or-fn args)))
	 nil)
	((null stream)
	 (with-output-to-string (stream)
	   (apply #'format stream string-or-fn args)))
	(T (if (eq stream T) (setq stream *standard-output*))
	   (when (stringp string-or-fn)
	     (setq string-or-fn (process-format-string string-or-fn nil)))
	   (cond ((not (stringp string-or-fn))
		  (apply string-or-fn stream args))
		 ((xp-structure-p stream)
		  (apply #'using-format stream string-or-fn args))
		 (T (apply #'lisp:format stream string-or-fn args)))
	   nil)))

(defun process-format-string (string-or-fn force-fn?)
  (cond ((not (stringp string-or-fn)) string-or-fn) ;called from ~? too.
	((not *format-string-cache*)
	 (maybe-compile-format-string string-or-fn force-fn?))
	(T (when (not (hash-table-p *format-string-cache*))
	     (setq *format-string-cache* (make-hash-table :test #'eq)))
	   (let ((value (gethash string-or-fn *format-string-cache*)))
	     (when (or (not value) (and force-fn? (stringp value)))
	       (setq value
		     (maybe-compile-format-string string-or-fn force-fn?))
	       (setf (gethash string-or-fn *format-string-cache*) value))
	     value))))

(defun write-char (char &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (write-char+ char stream) 
      (lisp:write-char char stream))
  char)

(defun write-string (string &optional (stream *standard-output*)
			    &key (start 0) (end (length string)))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (write-string+ string stream start end)
      (lisp:write-string string stream :start start :end end))
  string)

(defun write-line (string &optional (stream *standard-output*)
			  &key (start 0) (end (length string)))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (progn (write-string+ string stream start end)
	     (pprint-newline+ :unconditional stream))
      (lisp:write-line string stream :start start :end end))
  string)

(defun terpri (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (pprint-newline+ :unconditional stream)
      (lisp:terpri stream))
  nil)

;;; This has to violate the XP data abstraction and fool with internal
;;; stuff, in order to find out the right info to return as the result.
(defun fresh-line (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (cond ((xp-structure-p stream)
	 (attempt-to-output stream T T) ;ok because we want newline
	 (when (not (zerop (LP<-BP stream)))
	   (pprint-newline+ :fresh stream)
	   T))
	(T (lisp:fresh-line stream))))

;;; Each of these causes the stream to be pessimistic and insert
;;; newlines wherever it might have to, when forcing the partial output
;;; out.  This is so that things will be in a consistent state if
;;; output continues to the stream later.
(defun finish-output (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (attempt-to-output stream T T)
      (lisp:finish-output stream))
  nil)

(defun force-output (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (attempt-to-output stream T T)
      (lisp:force-output stream))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (let ((*locating-circularities* 0)) ;hack to prevent visible output
	(attempt-to-output stream T T))
      (lisp:clear-output stream))
  nil)

;;; Note we are assuming that if a structure is defined using xp::defstruct,
;;; then its print-function (if any) will be defined using xp::print etc.
(defmacro defstruct (name &body body)
  (let* ((struct-name (if (consp name) (car name) name))
	 (printer (cadr (safe-assoc :print-function name)))
	 (xp-print-fn
	  (intern (concatenate 'string
			       "PRINT-" (string (package-name
						 (symbol-package struct-name)))
			       ":" (string struct-name))
		  (find-package "XP"))))
    (cond (printer
	   `(eval-when (eval load compile)
	     (lisp:defstruct ,name ,@ body)
	     (defun ,xp-print-fn (xp obj)
	       (funcall (function ,printer) obj xp *current-level*))
	     (setf (get ',struct-name 'structure-printer) #',xp-print-fn)
	     ',(if (consp name) (car name) name)))
	  ((and (not (safe-assoc :type name))
		(not (safe-assoc :include name)))
	   (let* ((conc-name-spec (safe-assoc :conc-name name))
		  (conc-name
		   (cond ((null conc-name-spec)
			  (concatenate 'string (string struct-name) "-"))
			 ((null (cadr conc-name-spec)) "")
			 (T (string (cadr conc-name-spec)))))
		  (slots (mapcar
			  #'(lambda (x) (if (consp x) (car x) x))
			  body)))
	     `(eval-when (eval load compile)
	       (lisp:defstruct ,name ,@ body)
	       (defun ,xp-print-fn (xp obj)
		 (funcall (formatter
			   "~@<#S(~;~W ~:I~@_~@{:~A ~W~^ ~:_~}~;)~:>") xp
			  ',struct-name
			  ,@(mapcan
			     #'(lambda (slot)
				 `(,(string slot)
				   (,(intern (concatenate
					      'string
					      conc-name (string slot)))
				    obj)))
				    slots)))
	       (setf (get ',struct-name 'structure-printer) #',xp-print-fn)
	       ',(if (consp name) (car name) name))))
	  (T `(eval-when (eval load compile)
	       (setf (get ',struct-name 'structure-printer) :none)
	       (lisp:defstruct ,name ,@ body))))))

(defun safe-assoc (item list)
  (do ((l list (cdr l))) ((not (consp l)) nil)
    (if (and (consp (car l)) (eq (caar l) item)) (return (car l)))))

;;;           ---- FUNCTIONAL INTERFACE TO DYNAMIC FORMATTING ----
;;;
;;; The internal functions in this file, and the (formatter "...") expansions
;;; use the '+' forms of these functions directly (which is faster) because,
;;; they do not need error checking or fancy stream coercion.  The '++' forms
;;; ;additionally assume the thing being output does not contain a newline.

(defmacro pprint-logical-block ((stream-symbol list
					       &key
					       (prefix nil)
					       (per-line-prefix nil)
					       (suffix ""))
				&body body)
  (cond ((eq stream-symbol nil) (setq stream-symbol '*standard-output*))
	((eq stream-symbol T) (setq stream-symbol '*terminal-io*)))
  (when (not (symbolp stream-symbol))
    (warn "STREAM-SYMBOL arg ~S to PPRINT-LOGICAL-BLOCK is not a bindable symbol"
	  stream-symbol)
    (setq stream-symbol '*standard-output*))
  (when (and prefix per-line-prefix)
    (warn "prefix ~S and per-line-prefix ~S cannot both be specified ~
           in PPRINT-LOGICAL-BLOCK")
    (setq per-line-prefix nil))
  `(maybe-initiate-xp-printing
    #'(lambda (,stream-symbol)
	(let ((+l ,list)
	      (+p ,(or prefix per-line-prefix ""))
	      (+s ,suffix))
	  (pprint-logical-block+
	   (,stream-symbol +l +p +s ,(not (null per-line-prefix)) T nil)
	   ,@ body nil)))
    (decode-stream-arg ,stream-symbol)))

;;; Assumes var and args must be variables.  Other arguments must be literals or variables.

(defmacro pprint-logical-block+ ((var args prefix suffix per-line?
				      circle-check? atsign?)
				 &body body)
  (when (and circle-check? atsign?)
    (setq circle-check? 'not-first-p))
  `(let ((*current-level* (1+ *current-level*))
	 (*current-length* -1)
	 (*parents* *parents*)
	 ,@(if (and circle-check? atsign?)
	       `((not-first-p (plusp *current-length*)))))
    (unless (check-block-abbreviation ,var ,args ,circle-check?)
      (block logical-block
	(start-block ,var ,prefix ,per-line? ,suffix)
	(unwind-protect
	     (macrolet ((pprint-pop () `(pprint-pop+ ,',args ,',var))
			(pprint-exit-if-list-exhausted ()
			  `(if (null ,',args)
			    (return-from logical-block nil))))
	       ,@ body)
	  (end-block ,var ,suffix))))))

(defun pprint-newline (kind &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (not (member kind '(:linear :miser :fill :mandatory)))
    (error "Invalid KIND argument ~A to PPRINT-NEWLINE" kind))
  (when (xp-structure-p stream)
    (pprint-newline+ kind stream))
  nil)

(defun pprint-indent (relative-to n &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (not (member relative-to '(:block :current)))
    (error "Invalid KIND argument ~A to PPRINT-INDENT" relative-to))
  (when (xp-structure-p stream)
    (pprint-indent+ relative-to n stream))
  nil)

(defun pprint-tab (kind colnum colinc &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (not (member kind '(:line :section :line-relative :section-relative)))
    (error "Invalid KIND argument ~A to PPRINT-TAB" kind))
  (when (xp-structure-p stream)
    (pprint-tab+ kind colnum colinc stream))
  nil)

