;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun char-macro-close-paren (stream char)
  (declare (ignore stream char))
  (if (> *open-paren-count* 0)
      *close-paren-marker*
      (progn (warn "Ignoring an extra closing parenthesis~%")
	     (values))))

(defun char-macro-double-quote (stream char)
  (declare (ignore char))
  (read-string stream 0))

(defun char-macro-open-paren (stream char)
  (declare (ignore char))
  (let ((*open-paren-count* (+ *open-paren-count* 1)))
    (read-list stream)))

(defun char-macro-quote (stream char)
  (declare (ignore char))
  (list 'quote (read/4 stream t nil t)))

(defun char-macro-semicolon (stream char)
  (declare (ignore char))
  (loop for ch = (read-char stream nil #\Newline t)
	until (char= ch #\Newline))
  (values))

(defun copy-readtable (&optional (from-rt *readtable*) to-rt)
  (let ((new-rt (or to-rt (make-readtable))))
    (check-type new-rt readtable)
    (etypecase from-rt
      (null (init-default-readtable new-rt))
      (readtable
       (replace (readtable-syntax new-rt)
		(readtable-syntax from-rt))
       (replace (readtable-attributes new-rt)
		(readtable-attributes from-rt))
       (replace (readtable-handlers new-rt)
		(readtable-handlers from-rt))))
    new-rt))

(defun dispatch-macro-handler (stream disp-char)
  (loop for sub-char = (read-char stream t nil t)
	as digit? = (digit-char-p sub-char)
	when digit? collect sub-char into digits
	until (not digit?)
	finally
	(return
	  (let ((handler (get-dispatch-macro-character disp-char sub-char)))
	    (if (null handler)
		(error "The dispatch macro sequence ~C~C is undefined"
		       disp-char sub-char)
		(let ((n (if (null digits)
			     nil
			     (parse-integer (coerce-to-vector digits) 10))))
		  (funcall handler stream sub-char n)))))))

(defun eval-conditional-read (subchar feature)
  (if *read-suppress*
      nil
      (let ((flag (eval-conditional-read-1 feature)))
	(case subchar
	  (#\+ flag)
	  (#\- (not flag))))))
	
(defun eval-conditional-read-1 (feature)
  (if (consp feature)
      (ecase (car feature)
	(and (every #'eval-conditional-read-1 (cdr feature)))
	(or (some #'eval-conditional-read-1 (cdr feature)))
	(not (not (eval-conditional-read-1 (second feature)))))
      (feature? feature)))

(defun make-default-readtable ()
  (init-default-readtable (make-readtable)))

(defun init-default-readtable (rt)
  (set-char-syntax rt #\tab whitespace)
  (set-char-syntax rt #\space whitespace)
  (set-char-syntax rt #\page whitespace)
  (set-char-syntax rt #\newline whitespace)
  (set-char-syntax rt #\backspace whitespace)
  (set-char-syntax rt #\return whitespace)
  (set-char-syntax rt #\rubout whitespace)
  (set-char-syntax rt #\linefeed whitespace)
  (set-char-syntax rt #\\ single-escape)
  (set-char-syntax rt #\| multiple-escape)

  (set-char-attribute rt #\tab illegal)
  (set-char-attribute rt #\space illegal)
  (set-char-attribute rt #\page illegal)
  (set-char-attribute rt #\newline illegal)
  (set-char-attribute rt #\backspace illegal)
  (set-char-attribute rt #\return illegal)
  (set-char-attribute rt #\rubout illegal)
  (set-char-attribute rt #\linefeed illegal)
    
  (set-macro-character #\' #'char-macro-quote nil rt)	
  (set-macro-character #\; #'char-macro-semicolon nil rt)	
  (set-macro-character #\( #'char-macro-open-paren nil rt)
  (set-macro-character #\) #'char-macro-close-paren nil rt)
  (set-macro-character #\" #'char-macro-double-quote nil rt)
  (set-macro-character #\` #'char-macro-read-backquote nil rt)
  (set-macro-character #\, #'char-macro-read-comma nil rt)
  (make-dispatch-macro-character #\# t rt)

  (set-dispatch-macro-character #\# #\' #'sharp-macro-function rt)
  (set-dispatch-macro-character #\# #\\ #'sharp-macro-read-char rt)
  (set-dispatch-macro-character #\# #\+ #'sharp-macro-cond-read rt)
  (set-dispatch-macro-character #\# #\- #'sharp-macro-cond-read rt)
  (set-dispatch-macro-character #\# #\, #'sharp-macro-eval rt)
  (set-dispatch-macro-character #\# #\: #'sharp-macro-read-uninterned rt)
  (set-dispatch-macro-character #\# #\X #'sharp-macro-read-in-base rt)
  (set-dispatch-macro-character #\# #\x #'sharp-macro-read-in-base rt)
  (set-dispatch-macro-character #\# #\O #'sharp-macro-read-in-base rt)
  (set-dispatch-macro-character #\# #\o #'sharp-macro-read-in-base rt)
  (set-dispatch-macro-character #\# #\B #'sharp-macro-read-in-base rt)
  (set-dispatch-macro-character #\# #\b #'sharp-macro-read-in-base rt)
  (set-dispatch-macro-character #\# #\( #'sharp-macro-read-vector rt)
  (set-dispatch-macro-character #\# #\A #'sharp-macro-read-array rt)
  (set-dispatch-macro-character #\# #\a #'sharp-macro-read-array rt)
  (set-dispatch-macro-character #\# #\C #'sharp-macro-read-complex rt)
  (set-dispatch-macro-character #\# #\c #'sharp-macro-read-complex rt)
  (set-dispatch-macro-character #\# #\* #'sharp-macro-read-bit-vector rt)
  (set-dispatch-macro-character #\# #\. #'sharp-macro-read-eval rt)
  (set-dispatch-macro-character #\# #\| #'sharp-macro-big-comment rt)
  (set-dispatch-macro-character #\# #\P #'sharp-macro-read-pathname rt)
  (set-dispatch-macro-character #\# #\p #'sharp-macro-read-pathname rt)
  (set-dispatch-macro-character #\# #\S #'sharp-macro-read-structure rt)
  (set-dispatch-macro-character #\# #\s #'sharp-macro-read-structure rt)

  rt)

(defun make-dispatch-macro-character (char &optional
					   non-terminating?
					   (rt *readtable*))
  (set-char-syntax rt char (if non-terminating?
			       non-terminating-macro
			       terminating-macro))
  (set-char-macro-info
   rt
   char
   (make-dispatch-macro-char
    :function #'dispatch-macro-handler
    :sub-functions (make-array 256
			       :initial-element #'undefined-sub-char)
    :non-terminating? non-terminating?))
  t)

(defun-inline parse-integer (string &key
				    (start 0)
				    (end (length string))
				    (radix 10)
				    (junk-allowed nil))
  (parse-integer-1 string start end radix junk-allowed))

(defun parse-integer-1 (string start end radix junk-allowed)
  (let* ((negate? (char= (aref string start) #\-))
	 (sign? (or negate? (char= (aref string start) #\+)))
	 (abs (loop for i from (1- end) downto (if sign? (1+ start) start)
		    as mult = 1 then (* mult radix)
		    summing (* (digit-char-p (aref string i) radix)
			       mult))))
    (if negate? (- abs) abs)))

(defun read/4 (stream eof-error-p eof-value recursivep)
  (let ((buffer nil)
	(index 0)
	(end 0)
	(escape? nil)
	(multiple? nil)
	(escaped-indices nil)
	(next nil))
    (declare (fixnum index end)
	     (type stream stream)
	     (type simple-string buffer))
    (macrolet ((char-syntax ()
		 `(get-char-syntax *readtable* next))
	       (eof? () '(eq next stream))
	       (init () `(setq buffer (get-string-buffer)
			       end (length buffer)))
	       (stuff (char)
		 `(progn (%set-schar buffer index ,char)
		   (setq index (+ index 1))
		   (when (= index end)
		     (error "Fix reader to extend buffer"))))
	       (stuff-escaped (char)
		 `(progn
		   (push index escaped-indices)
		   (stuff ,char))))
      (tagbody
       start
       eat-whitespace
	 (setq next (read-char-unsafe stream nil stream))
	 (when (eof?) (go handle-eof))
	 (select (char-syntax)
	   (whitespace (go eat-whitespace))
	   (constituient (init)
			 (stuff (char-upcase next))
			 (go even-multiple-escape))
	   (multiple-escape (init) (go odd-multiple-escape))
	   (single-escape (init) (go single-escape))
	   ((terminating-macro non-terminating-macro)
	    (go execute-character-macro)))

       execute-character-macro
	 (multiple-value-call #'(lambda (&optional (value nil supplied?))
				  (if supplied?
				      (return-from read/4 value)
				      (go start)))
	   (funcall (get-macro-character next) stream next))

       odd-multiple-escape
	 (setq escape? t)
	 (setq multiple? t)
	 (go build-token)

       single-escape
	 (setq escape? t)
	 (setq multiple? nil)
	 (go build-token)

       even-multiple-escape
	 (setq escape? nil)
	 (setq multiple? nil)

       build-token
	 (setq next (read-char-unsafe stream nil stream))
	 (if escape?
	     (progn (when (eof?) (go handle-eof))
		    (cond ((and (= (char-syntax) multiple-escape) multiple?)
			   (go even-multiple-escape))
			  ((and (= (char-syntax) single-escape) multiple?)
			   (setq next (read-char-unsafe stream nil stream))
			   (when (eof?) (go handle-eof))
			   (stuff-escaped next)
			   (go build-token))
			  (t (stuff-escaped next)
			     (unless multiple? (setq escape? nil))
			     (go build-token))))
	     (progn (when (eof?) (go end-token))
		    (select (char-syntax)
		      (whitespace (unread-char-unsafe next stream)
				  (go end-token))
		      ((constituient non-terminating-macro)
		       (stuff (char-upcase next))
		       (go build-token))
		      (multiple-escape (go odd-multiple-escape))
		      (single-escape (go single-escape))
		      (terminating-macro (unread-char-unsafe next stream)
					 (go end-token)))))

       end-token
	 (return-from read/4
	   (token->object (free-string-buffer buffer index) escaped-indices))

       handle-eof
	 ;; HEY! need at least one whitespace char before eof
	 (if eof-error-p
	     (error "Reached end of file in the middle of an expression")
	     (return-from read/4 eof-value))))))

(defun read-from-string (string)
  (with-input-from-string (stream string)
    (read stream)))

(defun read-line (&optional (stream *standard-input*)
			    (eof-error-p t) eof-value recursivep)
  (read-line-1 stream 0 eof-error-p eof-value))

(defun read-line-1 (stream len eof-error-p eof-value)
  (let ((char (read-char stream nil stream)))
    (cond ((eq char stream)
	   (if (= len 0)
	       (if eof-error-p
		   (error-stream-eof stream)
		   eof-value)
	       (make-string len)))
	  ((char= char #\newline) (make-string len))
	  (t (let ((string
		    (read-line-1 stream (+ len 1) eof-error-p eof-value)))
	       (setf (schar string len) char)
	       string)))))

(defun read-list (stream)
  (let ((next (read/4 stream t nil t)))
    (select next
      (*close-paren-marker* nil)
      (*dot-marker*
       (let ((cdr (read/4 stream t nil t))
	     (close (read/4 stream t nil t)))
	 (unless (eq close *close-paren-marker*)
	   (error "A closing parenthesis is missing after a dot"))
	 cdr))
      (t (cons next (read-list stream))))))

(defun read-string (stream len)
  (let ((char (read-char stream nil stream t)))
    (if (char=/2 char #\")
	(make-string len)
	(let ((next (if (char=/2 char #\\)
			(read-char stream nil stream t)
			char)))
	  (let ((string (read-string stream (+ len 1))))
	    (setf (schar string len) next)
	    string)))))

(defun-inline read (&optional (stream *standard-input*)
			      (eof-error-p t) eof-value recursivep)
  (read-1 stream eof-error-p  eof-value recursivep))

(defun read-1 (stream eof-error-p  eof-value recursivep)
  (with-valid-stream (s stream)
    (read/4 s eof-error-p  eof-value recursivep)))

(defun set-char-attribute  (rt char value)
  (setf (get-char-attributes rt char) value))

(defun set-char-macro-info (rt char value)
  (setf (get-char-macro-info rt char) value))

(defun set-char-syntax (rt char value)
  (setf (get-char-syntax rt char) value))

(defun set-dispatch-macro-character (disp-char sub-char function
					       &optional (rt *readtable*))
  (let ((table (get-char-macro-info rt disp-char)))
    (if (dispatch-macro-char-p table)
	(setf (aref (dispatch-macro-char-sub-functions table)
		    (char-code sub-char))
	      function)
	(error "~C is not a dispatching macro character" disp-char)))
  t)

(defun set-macro-character (char function
				 &optional non-terminating? (rt *readtable*))
  (set-char-syntax rt char (if non-terminating?
			       non-terminating-macro
			       terminating-macro))
  (set-char-macro-info rt
		       char
		       (make-macro-char :function function
					:non-terminating? non-terminating?))
  t)

(defun set-syntax-from-char (to-char from-char &optional (rt *readtable*))
  to-char from-char rt
  (error "write me"))

;;; Adapted from SPICE
(defun sharp-macro-big-comment (stream subchar arg)
  (declare (ignore subchar arg))
  (do ((level 1)
       (prev (read-char stream) char)
       (char (read-char stream) (read-char stream)))
      (())
    (cond ((and (char= prev #\|) (char= char #\#))
	   (setq level (1- level))
	   (when (= level 0)
	     (return (values)))
	   (setq char (read-char stream)))
	  ((and (char= prev #\#) (char= char #\|))
	   (setq char (read-char stream))
	   (setq level (1+ level))))))

(defun sharp-macro-cond-read (stream subchar arg)
  (declare (ignore arg))
  (if (eval-conditional-read subchar (read/4 stream t nil t))
      (read/4 stream t nil t)
      (let ((*read-suppress* t))
	(read/4 stream t nil t)
	(values))))

(defun sharp-macro-eval (stream subchar arg)
  (declare (ignore subchar arg))
  (eval (read/4 stream t nil t)))

(defun sharp-macro-function (stream subchar arg)
  (declare (ignore subchar arg))
  (list 'function (read/4 stream t nil t)))

(defun sharp-macro-read-array (stream subchar rank)
  (let ((contents (read stream t nil t)))
    (labels ((collect-array-dims-from-contents-list (r c)
	       (cond ((= r 0) nil)
		     ((listp c)
		      (cons (length c)
			    (collect-array-dims-from-contents-list (1- r)
								    (car c))))
		     (t (error "~A is an illegal contents list for ~
                               an array of rank ~D" contents rank)))))
      (make-array (collect-array-dims-from-contents-list rank contents)
		  :initial-contents contents))))

(defun sharp-macro-read-bit-vector (stream subchar length)
  (declare (ignore length))
  (let* ((*read-base* 2)
	 (number (read stream t nil t)))
    (if (numberp number)
	(let* ((initial-bits (write-to-string number :base 2))
	       (len (length initial-bits))
	       (vector (make-array len :element-type 'bit)))
	  (loop for i from 0 below len
		do (setf (sbit vector i) (case (schar initial-bits i)
					   (#\0 0)
					   (#\1 1))))
	  vector)
	(error "#~C error, ~A is not a binary number"
	       subchar number))))

(defun sharp-macro-read-char (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((char (read-char stream t nil t)))
    (if (alpha-char-p char)
	(progn (unread-char char stream)
	       (let ((sym (read/4 stream t nil t)))
		 (if *read-suppress*
		     nil
		     (if (= (length (symbol-name sym)) 1)
			 char
			 (let ((c (name-char sym)))
			   (if (null c) 
			       (error "~A is not a recognized character name"
				      sym)
			       c))))))
	char)))

(defun sharp-macro-read-complex (stream subchar n)
  (declare (ignore n))
  (let ((list (read stream t nil t)))
    (if (listp list)
	(complex (first list) (second list))
	(error "~A is not legal syntax after #~C" list subchar))))

(defun sharp-macro-read-eval (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((object (read stream t nil t)))
    (if *read-suppress*
	nil
	(if *read-eval*
	    (eval object)
	    (error "*READ-EVAL* is false, so read time eval is illegal")))))

(defun sharp-macro-read-in-base (stream subchar arg)
  (declare (ignore arg))
  (let ((*read-base* (ecase (char-upcase subchar)
		       (#\X 16.)
		       (#\O 8.)
		       (#\B 2.))))
    (read/4 stream nil t t)))

(defun sharp-macro-read-pathname (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((name (read stream t nil t)))
    (if (stringp name)
	(pathname name)
	(error "~A is not a legal pathname" name))))

(defun sharp-macro-read-structure (stream subchar arg)
  (let* ((l (read stream t stream t))
	 (struct-info (lookup-structure-info (car l))))
    (when (null struct-info)
      (error "~A is not a structure type" (car l)))
    (loop for cell on (cdr l) by #'cddr
	  do (setf (car cell) (intern (symbol-name (car cell))
				      *keyword-package*)))
    (let ((constructor (struct-info-constructor struct-info)))
      (if (null constructor)
	  (error "Structure ~S does not have a constructor" (car l))
	  (apply constructor (cdr l))))))

(defun sharp-macro-read-uninterned (stream subchar arg)
  (declare (ignore subchar arg))
  ;;; This is still broken, but better than before when the symbol stayed
  ;;; interned. Still need a way to read without interning at all.
  (let ((symbol (read stream t nil t)))
    (unintern symbol)
    symbol))

(defun sharp-macro-read-vector (stream subchar length)
  (declare (ignore length))
  (unread-char subchar stream)
  (let ((list (read stream t nil t)))
    (if (listp list)
	(coerce list 'vector)
	(error "~A is not legal syntax after #~C" list subchar))))

(defun token->float (buffer exponent-start)
  (let ((exponent (if (null exponent-start)
		      0
		      (progn (setf (schar buffer (1- exponent-start))
				   #\space) ; delimiter for strtod
			     (parse-integer buffer :start exponent-start)))))
    (* (strtod buffer 0)
       (expt 10 exponent))))

;;; HEY! This is obsolete. Replace all refs with PARSE-INTEGER.
(defun token->integer (buffer radix)
  (parse-integer buffer :radix radix))

(defun token->object (token escaped-indices)
  (let ((pos 0)
	(next nil)
	(exponent-pos nil)
	(len (length token)))
    (declare (fixnum pos len))
    (macrolet ((transit (state)
		 `(progn
		   (setq next (%schar token pos))
		   (setq pos (%+ pos 1))
		   (go ,state)))
	       (sign? ()
		 '(or (char= next #\+) (char= next #\-)))
	       (digit? ()
		 '(digit-char-p next *read-base*))
	       (slash? ()
		 '(char= next #\/))
	       (dot? ()
		 '(char= next #\.))
	       (exp-marker? ()
		 '(or (char= next #\E) (char= next #\e)
		   (char= next #\S) (char= next #\s)
		   (char= next #\F) (char= next #\f)
		   (char= next #\L) (char= next #\l)
		   (char= next #\D) (char= next #\d)))
	       (end? ()
		 '(> pos len)))
      (tagbody
	 (transit start)

       start
	 (cond ((end?) (error "empty token"))
	       ((digit?) (transit number))
	       ((sign?) (transit single-sign))
	       ((dot?) (transit single-dot))
	       (t (go accept-symbol)))

       single-sign
	 (cond ((end?) (go accept-symbol))
	       ((digit?) (transit number))
	       ((dot?) (transit single-dot))
	       (t (go accept-symbol)))
     
       number
	 (cond ((end?) (go accept-integer))
	       ((digit?) (transit number))
	       ((slash?) (transit ratio))
	       ((dot?) (transit number-dot))
	       ((exp-marker?) (transit float-exp))
	       (t (go accept-symbol)))

       ratio
	 (cond ((end?) (go accept-ratio))
	       ((digit?) (transit ratio))
	       (t (go accept-symbol)))

       single-dot
	 (cond ((end?) (if (> *open-paren-count* 0)
			   (return-from token->object *dot-marker*)
			   (go dot-only-token-error)))
	       ((digit?) (transit float-dp))
	       ((dot?) (transit multiple-dots))
	       (t (go accept-symbol)))
	 
       dot-only-token-error
	 (if (null escaped-indices)
	     (error "Symbols consisting of only dots are illegal")
	     (go accept-symbol))

       multiple-dots
	 (cond ((end?) (go dot-only-token-error))
	       ((dot?) (transit multiple-dots))
	       (t (go accept-symbol)))

       number-dot
	 (cond ((end? (go accept-integer-base-10)))
	       ((digit?) (transit float-dp))
	       (t (go accept-integer-base-10)))

       accept-integer-base-10
	 (return-from token->object
	   (parse-integer token :end (1- (length token)) :radix 10))

       float-dp
	 (cond ((end?) (go accept-float))
	       ((digit?) (transit float-dp))
	       ((exp-marker?) (transit float-exp))
	       (t (go accept-symbol)))

       float-exp
	 (setq exponent-pos (%- pos 1))
	 (cond ((sign?) (transit float-exp-sign))
	       ((digit?) (transit float-exp-digits))
	       (t (go accept-symbol)))

       float-exp-sign
	 (cond ((digit?) (transit float-exp-digits))
	       (t (go accept-symbol)))

       float-exp-digits
	 (cond ((end?) (go accept-float))
	       ((digit?) (transit float-exp-digits))
	       (t (go accept-symbol)))

       accept-integer
	 (return-from token->object (token->integer token *read-base*))

       accept-ratio
	 (return-from token->object (token->ratio token))

       accept-float
	 (return-from token->object (token->float token exponent-pos))

       accept-symbol
	 (return-from token->object (token->symbol token escaped-indices))))))

(defun token->ratio (buffer)
  (let ((slash (position #\/ buffer)))
    (make-ratio (parse-integer buffer :end slash)
		(parse-integer buffer :start (1+ slash)))))


(defun token->symbol (buffer escaped-indices)
  (declare (simple-string buffer))
  (unless *read-suppress*
    (let* ((len (length buffer))
	   (colon-pos (dotimes (i len nil)
			(declare (fixnum i))
			(when (and (char= (%schar buffer i) #\:)
				   (not (member i escaped-indices)))
			  (return i)))))
      (declare (fixnum len colon-pos))
      (if (null colon-pos)
	  (values (intern/2 buffer *package*))
	  (let* ((package
		  (if (= colon-pos 0)
		      *keyword-package*
		      (loop with package-string = (make-string colon-pos)
			    for i from 0 below colon-pos
			    do (setf (schar package-string i)
				     (schar buffer i))
			    finally (return
				      (coerce-to-package package-string)))))
		 (next-colon-pos (%+ colon-pos 1))
		 (double-colon? (and (char= (schar buffer next-colon-pos) #\:)
				     (not (member next-colon-pos
						  escaped-indices))))
		 (token-start (%+ colon-pos (if double-colon? 2 1)))
		 (name (make-string (%- len token-start))))
	    (declare (fixnum token-start))
	    (loop for src from token-start below len
		  for dest from 0 below len
		  do (setf (schar name dest) (schar buffer src)))
	    (multiple-value-bind (symbol type) (intern/2 name package)
	      (if double-colon?
		  symbol		     
		  (if (eq type :internal)
		      (error "The symbol ~A is internal, not external, to ~A"
		       name package)
		      symbol))))))))

(defun undefined-sub-char (stream sub-char arg)
  (declare (ignore arg stream))
  (error "Subchar ~C does not have a handler defined" sub-char))

(defun get-dispatch-macro-character (disp-char sub-char
					       &optional (rt *readtable*))
  (let ((table (get-char-macro-info rt disp-char)))
    (if (dispatch-macro-char-p table)
	(values (aref (dispatch-macro-char-sub-functions table)
		      (char-code sub-char))
		(dispatch-macro-char-non-terminating? table))
	(error "~C is not a dispatching macro character" disp-char))))

(defun get-macro-character (char &optional (rt *readtable*))
  (let ((info (get-char-macro-info rt char)))
    ;; HEY! structure subtypes have worked for a long time now,
    ;; fix this bootstrapping junk
    (cond ((macro-char-p info)
	   (values (macro-char-function info)
		   (macro-char-non-terminating? info)))
	  ((dispatch-macro-char-p info)
	   (values (dispatch-macro-char-function info)
		   (dispatch-macro-char-non-terminating? info)))
	nil)))

