;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Functions to implement FORMAT for Spice Lisp.
;;;
;;; Original by David Adam.
;;; Re-write by Bill Maddox.

;;; Hacked up version of Spice FORMAT.

(eval-when (compile)

;;; MACROS

;;; This macro establishes the correct environment for processing
;;; an indirect control string.  CONTROL-STRING is the string to
;;; process, and FORMS are the forms to do the processing.  They 
;;; invariably will involve a call to SUB-FORMAT.  CONTROL-STRING
;;; is guaranteed to be evaluated exactly once.

  (defmacro format-with-control-string (control-string &body forms)
    `(let ((string (if (simple-string-p ,control-string)
		       ,control-string
		       (coerce ,control-string 'simple-string))))
      (declare (simple-string string))
      (let ((error (catch 'format-error
		     (let ((*format-control-string* string)
			   (*format-length* (length string))
			   (*format-index* 0))
		       (declare (simple-string *format-control-string*)
				(fixnum *format-length* *format-index*))
		       ,@forms
		       nil))))
	(when error
	  (throw 'format-error
	    (cons (list "While processing indirect control string~%~S~%~V@T^"
			*format-control-string*
			(1+ *format-index*))
		  error))))))

  
;;;; WITH-FORMAT-PARAMETERS, and other useful macros.
;;; This macro rebinds collects output to the standard output stream
;;; in a string.  For efficiency, we avoid consing a new stream on
;;; every call.  A stack of string streams is maintained in order to
;;; guarantee re-entrancy.

  (defmacro format-stringify-output (&body forms)
    `(let ((*standard-output*
	    (if *format-stream-stack*
		(pop *format-stream-stack*)
		(make-string-output-stream))))
      (unwind-protect
	   (progn ,@forms
		  (prog1
		      (get-output-stream-string *standard-output*)
		    (push *standard-output* *format-stream-stack*)))
	(get-output-stream-string *standard-output*))))

;;; Pops an argument from the current argument list.  This is either the
;;; list of arguments given to the top-level call to FORMAT, or the argument
;;; list for the current iteration in a ~{~} construct.  An error is signalled
;;; if the argument list is empty.

  (defmacro pop-format-arg ()
    '(if *format-arguments*
      (pop *format-arguments*)
      (format-error "Missing argument")))


;;; This macro decomposes the argument list returned by PARSE-FORMAT-OPERATION.
;;; PARMVAR is the list of parameters.  PARMDEFS is a list of lists of the form
;;; (<var> <default>).  The FORMS are evaluated in an environment where each 
;;; <var> is bound to either the value of the parameter supplied in the 
;;; parameter list, or to its <default> value if the parameter was omitted or
;;; explicitly defaulted.

  (defmacro with-format-parameters (parmvar parmdefs &body forms)
    (do ((parmdefs parmdefs (cdr parmdefs))
	 (bindings () (cons `(,(caar parmdefs) (or (if ,parmvar (pop ,parmvar))
						,(cadar parmdefs)))
			    bindings)))
	((null parmdefs)
	 `(let ,(nreverse bindings)
	   (when ,parmvar
	     (format-error "Too many parameters"))
	   ,@forms))))


  
;;;; Control String Parsing 

;;; The current control string is kept in *format-control-string*. 
;;; The variable *format-index* is the position of the last character
;;; processed, indexing from zero.  The variable *format-length* is the
;;; length of the control string, which is one greater than the maximum
;;; value of *format-index*.  


;;; Gets the next character from the current control string.  It is an
;;; error if there is none.  Leave *format-index* pointing to the
;;; character returned.

  (defmacro nextchar ()
    '(if (< (the fixnum (incf (the fixnum *format-index*)))
	  (the fixnum *format-length*))
      (schar *format-control-string* *format-index*)
      (format-error "Syntax error")))


;;; Returns the current character, i.e. the one pointed to by *format-index*.

  (defmacro format-peek ()
    '(schar *format-control-string* *format-index*))


;;; Returns the index of the first occurrence of the specified character
;;; between indices START (inclusive) and END (exclusive) in the control
;;; string.


  (defmacro format-find-char (char start end)
    `(position ,char (the simple-string *format-control-string*)
      :start ,start :end ,end :test #'char=))

  (defmacro parse-format-operation-modifier ()
    `(let ((temp (format-peek)))
      (cond ((char= temp #\:)
	     (nextchar)
	     (setf colon-p t))
	    ((char= temp #\@)
	     (nextchar)
	     (setf atsign-p t)))))

;;; Hairy dispatch-table initialization macro.  Takes a list of two-element
;;; lists (<character> <function-object>) and returns a vector char-code-limit
;;; elements in length, where the Ith element is the function associated with
;;; the character with char-code I.  If the character is case-convertible, it
;;; must be given in only one case; however, an entry in the vector will be
;;; made for both.
  (defmacro make-dispatch-vector (&body entries)
    (let ((entries (mapcan #'(lambda (x)
			       (let ((lower (char-downcase (car x)))
				     (upper (char-upcase (car x))))
				 (if (char= lower upper)
				     (list x)
				     (list (cons upper (cdr x))
					   (cons lower (cdr x))))))
			   entries)))
      (do ((entries (sort entries #'(lambda (x y) (char< (car x) (car y)))))
	   (charidx 0 (1+ charidx))
	   (comtab () (cons (if entries
				(if (= (char-code (caar entries)) charidx)
				    (cadr (pop entries))
				    nil)
				nil)
			    comtab)))
	  ((= charidx 128)		; was char-code-limit
	   (if entries
	       (error "Garbage in dispatch vector - ~S" entries))
	   `(loop with v = (make-array 128)
	     for i from 0 below 128
	     for x in ',(nreverse comtab)
	     do (setf (svref v i) x)
	     finally (return v))))))
  )

(defun flonum-to-string (x &optional width fdigits scale fmin)
  (write-to-string x))

(defun whitespace-char-p (char)
  "Determines whether or not the character is considered whitespace."
  (or (char= char #\space)
      (char= char #\tab)
      (char= char #\return)
      (char= char #\linefeed)))

(defun line-length () 80)

;;; Given a non-negative floating point number, SCALE-EXPONENT returns a
;;; new floating point number Z in the range (0.1, 1.0] and and exponent
;;; E such that Z * 10^E is (approximately) equal to the original number.
;;; There may be some loss of precision due the floating point representation.
;;;
(defun scale-exponent (x)
  (scale-expt-aux x 0.0 1.0 10.0 0.1 float-log10-of-2))

(defun scale-expt-aux (x zero one ten one-tenth log10-of-2)
  (multiple-value-bind (sig exponent)
		       (decode-float x)
    (declare (ignore sig))
    (if (= x zero)
	(values zero 1)
	(let* ((ex (round (* exponent log10-of-2)))
	       (x (if (minusp ex)		;For the end ranges.
		      (* x ten (expt ten (- -1 ex)))
		      (/ x ten (expt ten (1- ex))))))
	  (do ((d ten (* d ten))
	       (y x (/ x d))
	       (ex ex (1+ ex)))
	      ((< y one)
	       (do ((m ten (* m ten))
		    (z y (* z m))
		    (ex ex (1- ex)))
		   ((>= z one-tenth) (values z ex)))))))))

;;;; ERRORS
;;; Since errors may occur while an indirect control string is being
;;; processed, i.e. by ~? or ~{~:}, some sort of backtrace is necessary
;;; in order to indicate the location in the control string where the
;;; error was detected.  To this end, errors detected by format are
;;; signalled by throwing a list of the form ((control-string args))
;;; to the tag FORMAT-ERROR.  This throw will be caught at each level
;;; of indirection, and the list of error messages re-thrown with an
;;; additional message indicating that indirection was present CONSed
;;; onto it.  Ultimately, the last throw will be caught by the top level
;;; FORMAT function, which will then signal an error to the Lisp error
;;; system in such a way that all the errror messages will be displayed
;;; in reverse order.

(defun format-error (complaint &rest args)
  (throw 'format-error
	 (list (list "~1{~:}~%~S~%~V@T^" complaint args
		     *format-control-string* (1+ *format-index*)))))



;;; Attempts to parse a parameter, starting at the current index.
;;; Returns the value of the parameter, or NIL if none is found. 
;;; On exit, *format-index* points to the first character which is
;;; not a part of the recognized parameter.
(defun format-get-parameter ()
  (case (format-peek)
    (#\# (nextchar) (length (the list *format-arguments*)))
    ((#\V #\v) (prog1 (pop-format-arg) (nextchar)))
    (#\' (prog1 (nextchar) (nextchar)))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (do* ((number (digit-char-p (format-peek))
		   (+ (* 10 number) (digit-char-p (format-peek)))))
	  ((not (digit-char-p (nextchar))) number)))
    (#\-
     (nextchar)
     (case (format-peek)
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	(do* ((number (digit-char-p (format-peek))
		      (+ (* 10 number) (digit-char-p (format-peek)))))
	     ((not (digit-char-p (nextchar))) (- number))))
       (t (decf (the fixnum *format-index*))  ; put back to out of place "-"
	  nil)))
    (#\+
     (nextchar)
     (case (format-peek)
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	(do* ((number (digit-char-p (format-peek))
		      (+ (* 10 number) (digit-char-p (format-peek)))))
	     ((not (digit-char-p (nextchar))) number)))
       (t (decf (the fixnum *format-index*))  ; put back to out of place "-"
	  nil)))
    (t nil)))

(defun parse-format-operation ()
  (let* ((ch (nextchar))
	 (parms (if (or (digit-char-p ch)
			(member ch '(#\, #\# #\V #\v #\' #\+ #\-)
				:test #'char=))
		    (do ((parms (list (format-get-parameter))
				(cons (format-get-parameter) parms)))
			((char/= (format-peek) #\,) (nreverse parms))
		      (declare (list parms))
		      (nextchar))
		    '()))
	 colon-p atsign-p)
    (parse-format-operation-modifier)
    (parse-format-operation-modifier)
    (values parms colon-p atsign-p (format-peek))))

;;; Starting at the current value of *format-index*, finds the first
;;; occurrence of one of the specified directives. Embedded constructs,
;;; i.e. those inside ~(~), ~[~], ~{~}, or ~<~>, are ignored.  And error is
;;; signalled if no satisfactory command is found.  Otherwise, the
;;; following are returned as multiple values:
;;;
;;;     The value of *format-index* at the start of the search
;;;     The index of the "~" character preceding the command
;;;     The parameter list of the command
;;;     The ":" flag
;;;     The "@" flag
;;;     The command character
;;;
;;; Implementation note:  The present implementation is not particulary
;;; careful with storage allocation.  It would be a good idea to have
;;; a separate function for skipping embedded constructs which did not
;;; bother to cons parameter lists and then throw them away.
;;;
;;; We go to some trouble here to use POSITION for most of the searching.
;;;
;;; Another note: *FORMAT-ARGUMENTS* is let bound here so that we can
;;; guarantee that that list is not changed by FORMAT-FIND-COMMAND.
;;; This is necessary since PARSE-FORMAT-OPERATION (called below) calls
;;; FORMAT-GET-PARAMETER.  If the parameter is #\V or #\v then
;;; FORMAT-GET-PARAMETER will pop *FORMAT-ARGUMENTS*.  This causes the
;;; argument to be lost when we actually go to do the real formatting.
;;;
(defun format-find-command (command-list)
  (let ((start *format-index*)
	(*format-arguments* *format-arguments*))
    (do ((place start *format-index*)
	 (tilde (format-find-char #\~ start *format-length*)
		(format-find-char #\~ place *format-length*)))
	((not tilde)
	 (format-error "Expecting one of ~S" command-list))
      (setq *format-index* tilde)
      (multiple-value-bind (parms colon atsign command)
			   (parse-format-operation)
	(when (member command command-list :test #'char=)
	  (return (values start tilde parms colon atsign command)))
	(case command
	  (#\{ (nextchar)(format-find-command '(#\})))
	  (#\< (nextchar)(format-find-command '(#\>)))
	  (#\( (nextchar)(format-find-command '(#\))))
	  (#\[ (nextchar)(format-find-command '(#\])))
	  ((#\} #\> #\) #\])
	   (format-error "No matching bracket")))))))

 

;;;; This is the FORMAT top-level function.
(defun format (destination control-string &rest format-arguments)
  (declare (dynamic-extent format-arguments))
  (let ((*format-original-arguments* format-arguments) ;for abs. and rel. goto
	(*format-arguments* format-arguments)
	(*print-radix* nil)
	(*format-control-string*
	 (if (simple-string-p control-string)
	     control-string
	     (coerce control-string 'simple-string))))
    (declare (simple-string *format-control-string*))
    (cond
      ((not destination)
       (format-stringify-output
	(let ((errorp (catch 'format-error
			(catch 'format-escape
			  (catch 'format-colon-escape
			    (sub-format 0 (length *format-control-string*))))
			nil)))
	  (when errorp
	    (error "~%~:{~@?~%~}" (nreverse errorp))))))
      ((and (stringp destination) (vector-with-fill-pointer-p destination))
       (with-output-to-string (*standard-output* destination)
	 (let ((errorp (catch 'format-error
			 (catch 'format-escape
			   (catch 'format-colon-escape
			     (sub-format 0 (length *format-control-string*))))
			 nil)))
	   (when errorp
	     (error "~%~:{~@?~%~}" (nreverse errorp)))
	   nil)))
      (t
       (let ((*standard-output*
	      (if (or (eq destination 't)
		      (and (synonym-stream-p destination)
			   (eq (synonym-stream-symbol destination)
			       '*standard-output*)))
		  *standard-output*
		  destination)))
	 (let ((errorp (catch 'format-error
			 (catch 'format-escape
			   (catch 'format-colon-escape
			     (sub-format 0 (length *format-control-string*))))
			 nil)))
	   (when errorp
	     (error "~%~:{~@?~%~}" (nreverse errorp))))
	 nil)))))

;;;; SUB-FORMAT, the real work of FORMAT.

;;; This function does the real work of format.  The segment of the control
;;; string between indiced START (inclusive) and END (exclusive) is processed
;;; as follows: Text not part of a directive is output without further
;;; processing.  Directives are parsed along with their parameters and flags,
;;; and the appropriate handlers invoked with the arguments COLON, ATSIGN, and
;;; PARMS. 
;;;
;;; Implementation Note: FORMAT-FIND-CHAR uses the POSITION stream operation
;;; for speed.  This is potentially faster than character-at-a-time searching.

(defun sub-format (start end)
  (declare (fixnum start end))
  (let ((*format-index* start)
	(*format-length* end))
    (declare (fixnum *format-index* *format-length*))
    (do* ((place start *format-index*)
	  (tilde (format-find-char #\~ start end)
		 (format-find-char #\~ place end)))
	 ((not tilde)
	  (write-string *format-control-string*	*standard-output*
			:start place :end end))
      (declare (fixnum place tilde))
      (when (> tilde place)
	(write-string *format-control-string* *standard-output*
		      :start place :end tilde))
      (setq *format-index* tilde)
      (multiple-value-bind
       (parms colon atsign command)
       (parse-format-operation)
       (let ((cmdfun (svref *format-dispatch-table* (char-code command))))
	 (if cmdfun
	     (funcall cmdfun colon atsign parms)
	     (format-error "Illegal FORMAT command ~~~S" command))))
      (unless (< (the fixnum (incf (the fixnum *format-index*))) end)
	(return)))))



;;;; Conditional case conversion  ~( ... ~)

(defun format-capitalization (colon atsign parms)
  (when parms
    (format-error "No parameters allowed to ~~("))
  (nextchar)
  (multiple-value-bind
   (prev tilde end-parms end-colon end-atsign)
   (format-find-command '(#\)))
   (when (or end-parms end-colon end-atsign)
     (format-error "Flags or parameters not allowed"))
   (let ((string (format-stringify-output (sub-format prev tilde))))
     (declare (string string))
     (write-string
      (cond ((and atsign colon)
	     (nstring-upcase string))
	    (colon
	     (nstring-capitalize string))
	    (atsign
	     (let ((strlen (length string)))
	       (declare (fixnum strlen))
	       ;; Capitalize the first word only
	       (nstring-downcase string)
	       (do ((i 0 (1+ i)))
		   ((or (<= strlen i) (alpha-char-p (char string i)))
		    (setf (char string i) (char-upcase (char string i)))
		    string)
		 (declare (fixnum i)))))
	    (t (nstring-downcase string)))))))



;;; Up and Out (Escape)  ~^

(defun format-escape (colon atsign parms)
  (when atsign
    (format-error "FORMAT command ~~~:[~;:~]@^ is undefined" colon))
  (when (if (first parms)
	    (if (second parms)
		(if (third parms)
		    (typecase (second parms)
		      (integer
		       (<= (first parms) (second parms) (third parms)))
		      (character
		       (char< (first parms) (second parms) (third parms)))
		      (t nil))
		    (equal (first parms) (second parms)))
		(zerop (first parms)))
	    (not *format-arguments*))
    (throw (if colon 'format-colon-escape 'format-escape) nil)))


;;;; Conditional expression  ~[ ... ]


;;; ~[ 

(defun format-untagged-condition ()
  (let ((test (pop-format-arg)))
    (unless (integerp test)
      (format-error "Argument to ~~[ must be integer - ~S" test))
    (do ((count 0 (1+ count)))
	((= count test)
	 (multiple-value-bind
	  (prev tilde parms colon atsign cmd)
	  (format-find-command '(#\; #\]))
	  (declare (ignore colon))
	  (when atsign
	    (format-error "Atsign flag not allowed"))
	  (when parms
	    (format-error "No parameters allowed"))
	  (sub-format prev tilde)
	  (unless (char= cmd #\])
	    (format-find-command '(#\])))))
      (multiple-value-bind
       (prev tilde parms colon atsign cmd)
       (format-find-command '(#\; #\]))
       (declare (ignore prev tilde))
       (when atsign
	 (format-error "Atsign flag not allowed"))
       (when parms
	 (format-error "Parameters not allowed"))
       (when (char= cmd #\]) (return))
       (when colon
	 (nextchar)
	 (multiple-value-bind (prev tilde parms colon atsign cmd)
			      (format-find-command '(#\; #\]))
	   (declare (ignore parms colon atsign))
	   (sub-format prev tilde)
	   (unless (char= cmd #\])
	     (format-find-command '(#\]))))
	 (return))
       (nextchar)))))


;;; ~@[

(defun format-funny-condition ()
  (multiple-value-bind
   (prev tilde parms colon atsign)
   (format-find-command '(#\]))
   (when (or colon atsign parms)
     (format-error "Flags or arguments not allowed"))
   (if *format-arguments*
       (if (car *format-arguments*)
	   (sub-format prev tilde)
	   (pop *format-arguments*))
       (format-error "Missing argument"))))


;;; ~:[ 

(defun format-boolean-condition ()
  (multiple-value-bind
   (prev tilde parms colon atsign)
   (format-find-command '(#\;))
   (when (or parms colon atsign)
     (format-error "Flags or parameters not allowed"))
   (nextchar)			  
   (if (pop-format-arg)
       (multiple-value-bind
	(prev tilde parms colon atsign)
	(format-find-command '(#\]))
	(when (or colon atsign parms)
	  (format-error "Flags or parameters not allowed"))
	(sub-format prev tilde))
       (progn
	(sub-format prev tilde)
	(format-find-command '(#\]))))))


(defun format-condition (colon atsign parms)
  (when parms
    (push (pop parms) *format-arguments*)
    (unless (null parms)
      (format-error "Too many parameters to ~[")))
  (nextchar)
  (cond (colon
	 (when atsign
	   (format-error  "~~:@[ undefined"))
	 (format-boolean-condition))
	(atsign
	 (format-funny-condition))
	(t (format-untagged-condition))))


;;;; Iteration  ~{ ... ~}

(defun format-iteration (colon atsign parms)
  (with-format-parameters parms ((max-iter -1))
    (nextchar)
    (multiple-value-bind
     (prev tilde end-parms end-colon end-atsign)
     (format-find-command '(#\}))
     (when (or end-atsign end-parms)
       (format-error "Illegal terminator for ~~{"))
     (if (= prev tilde)
	 ;; Use an argument as the control string if ~{~} is empty
	 (let ((string (pop-format-arg)))
	   (unless (stringp string)
	     (format-error "Control string is not a string"))
	   (format-with-control-string string
	     (format-do-iteration 0 *format-length*
				  max-iter colon atsign end-colon)))
	 (format-do-iteration prev tilde max-iter colon atsign end-colon)))))


;;; The two catch tags FORMAT-ESCAPE and FORMAT-COLON-ESCAPE are needed here
;;; to correctly implement ~^ and ~:^.  The former aborts only the current
;;; iteration, but the latter aborts the entire iteration process.

(defun format-do-iteration (start end max-iter colon atsign at-least-once-p)
  (catch 'format-colon-escape
    (catch 'format-escape
      (if atsign
	  (do* ((count 0 (1+ count)))
	       ((or (= count max-iter)
		    (and (null *format-arguments*)
			 (if (= count 0) (not at-least-once-p) t))))
	    (catch 'format-escape
	      (if colon
		  (let* ((*format-original-arguments* (pop-format-arg))
			 (*format-arguments* *format-original-arguments*))
		    (unless (listp *format-arguments*)
		      (format-error "Argument must be a list"))
		    (sub-format start end))
		  (sub-format start end))))
	  (let* ((*format-original-arguments* (pop-format-arg))
		 (*format-arguments* *format-original-arguments*))
	    (unless (listp *format-arguments*)
	      (format-error "Argument must be a list"))
	    (do* ((count 0 (1+ count)))
		 ((or (= count max-iter)
		      (and (null *format-arguments*)
			   (if (= count 0) (not at-least-once-p) t))))
	      (catch 'format-escape
		(if colon
		    (let* ((*format-original-arguments* (pop-format-arg))
			   (*format-arguments* *format-original-arguments*))
		      (unless (listp *format-arguments*)
			(format-error "Argument must be a list of lists"))
		      (sub-format start end))
		    (sub-format start end)))))))))
  


;;;; Justification  ~< ... ~>

;;; Parses a list of clauses delimited by ~; and terminated by ~>.
;;; Recursively invoke SUB-FORMAT to process them, and return a list
;;; of the results, the length of this list, and the total number of
;;; characters in the strings composing the list.

(defun format-get-trailing-segments ()
  (nextchar)
  (multiple-value-bind
   (prev tilde colon atsign parms cmd)
   (format-find-command '(#\; #\>))
   (when colon
     (format-error "~~:; allowed only after first segment in ~~<"))
   (when (or atsign parms)
     (format-error "Flags and parameters not allowed"))
   (let ((str (catch 'format-escape
		(format-stringify-output (sub-format prev tilde)))))
     (declare (string str))
     (if str
	 (if (char= cmd #\;)
	     (multiple-value-bind
	      (segments numsegs numchars)
	      (format-get-trailing-segments)
	      (values (cons str segments)
		      (1+ numsegs) (+ numchars (length str))))
	     (values (list str) 1 (length str)))
	 (values () 0 0)))))


;;; Gets the first segment, which is treated specially.  Call 
;;; FORMAT-GET-TRAILING-SEGMENTS to get the rest.

(defun format-get-segments ()
  (multiple-value-bind
   (prev tilde parms colon atsign cmd)
   (format-find-command '(#\; #\>))
   (when atsign
     (format-error "Atsign flag not allowed"))
   (let ((first-seg (format-stringify-output (sub-format prev tilde))))
     (if (char= cmd #\;)
	 (multiple-value-bind
	  (segments numsegs numchars)
	  (format-get-trailing-segments)
	  (if colon
	      (values first-seg parms segments numsegs numchars)
	      (values nil nil (cons first-seg segments) (1+ numsegs)
		      (+ (length first-seg) numchars))))
	 (values nil nil (list first-seg) 1 (length first-seg))))))


   

;;;; Padding functions for Justification.

;;; Given the total number of SPACES needed for padding, and the number
;;; of padding segments needed (PADDINGS), returns a list of such segments.
;;; We try to allocate the spaces equally to each segment.  When this is
;;; not possible, allocate any left over spaces to the first segment.
;;;
(defun make-pad-segs (spaces padding-segs)
  (do* ((extra-space () (and (plusp extra-spaces)
			     extra-inc
			     (zerop (rem segs extra-inc))))
	(result () (cons (cond ((= segs 1) (+ min-space extra-spaces))
			       (extra-space (1+ min-space))
			       (t min-space))
			 result))
	(min-space (truncate spaces padding-segs))
	(extra-spaces (- spaces (* padding-segs min-space))
		      (if extra-space
			  (1- extra-spaces) extra-spaces))
	(extra-inc (if (plusp extra-spaces)
		       (truncate spaces extra-spaces)))
	(segs padding-segs (1- segs)))
       ((zerop segs) result)))
  

;;; Determine the actual width to be used for a field requiring WIDTH
;;; characters according to the following rule:  If WIDTH is less than or
;;; equal to MINCOL, use WIDTH as the actual width.  Otherwise, round up 
;;; to MINCOL + k * COLINC for the smallest possible positive integer k.
;;;
(defun format-round-columns (width mincol colinc)
  (if (> width mincol)
      (multiple-value-bind
       (quotient remainder)
       (floor (- width mincol) colinc)
       (+ mincol (* quotient colinc) (if (zerop remainder) 0 colinc)))
      mincol))



(defun format-justification (colon atsign parms)
  (with-format-parameters parms
    ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
    (unless (and (integerp mincol) (not (minusp mincol)))
      (format-error "Mincol must be a non-negative integer - ~S" mincol))
    (unless (and (integerp colinc) (plusp colinc))
      (format-error "Colinc must be a positive integer - ~S" colinc))
    (unless (and (integerp minpad) (not (minusp minpad)))
      (format-error "Minpad must be a non-negative integer - ~S" minpad))
    (unless (characterp padchar)
      (format-error "Padchar must be a character - ~S" padchar))
    (nextchar)
    (multiple-value-bind
	(special-arg special-parms segments numsegs numchars)
	(format-get-segments)
      (let* ((padsegs (+ (if (or colon (= numsegs 1)) 1 0)
			 (1- numsegs)
			 (if (and atsign (or (/= numsegs 1) colon))
			     1 0)))
	     (width (format-round-columns (+ numchars (* minpad padsegs))
					  mincol colinc))
	     (spaces (append (if (or colon (= numsegs 1)) () '(0))
			     (make-pad-segs (- width numchars) padsegs)
			     (if (and atsign (or (/= numsegs 1) colon))
				 () '(0)))))
	(when special-arg
	  (with-format-parameters special-parms ((spare 0)
						 (linel (or (line-length) 72)))
	    (let ((pos (or (stream-column *standard-output*) 0)))
	      (when (> (+ pos width spare) linel)
		(write-string special-arg)))))
	(cond ((and atsign (= numsegs 1) (not colon))
	       (write-string (car segments))
	       (dotimes (i (car spaces)) (write-char padchar)))
	      (t
	       (do ((segs segments (cdr segs))
		    (spcs spaces (cdr spcs)))
		   ((null segs) (dotimes (i (car spcs)) (write-char padchar)))
		 (dotimes (i (car spcs)) (write-char padchar))
		 (write-string (car segs)))))))))

;;; Fresh-line  ~%
(defun format-terpri (colon atsign parms)
  (when (or colon atsign)
    (format-error "Flags not allowed"))
  (with-format-parameters parms ((repeat-count 1))
    (dotimes (i repeat-count) (terpri))))


;;;; Newline  ~&
(defun format-freshline (colon atsign parms)
  (when (or colon atsign)
    (format-error "Flags not allowed"))
  (with-format-parameters parms ((repeat-count 1))
    (fresh-line)
    (dotimes (i (1- repeat-count)) (terpri))))


;;; Page  ~|

(defun format-page (colon atsign parms)
  (when (or colon atsign)
    (format-error "Flags not allowed"))
  (with-format-parameters parms ((repeat-count 1))
    (dotimes (i repeat-count) (write-char #\page))))


;;; Print a tilde  ~~

(defun format-tilde (colon atsign parms)
  (when (or colon atsign)
    (format-error "Flags not allowed"))
  (with-format-parameters parms ((repeat-count 1))
    (dotimes (i repeat-count) (write-char #\~))))


;;; Continue control string on next line  ~<newline>

(defun format-eat-whitespace ()
  (nextchar)
  (setq *format-index*
	(1- (the fixnum
		 (position-if-not #'(lambda (ch) (or (whitespace-char-p ch)
						     (char= ch #\linefeed)))
				  (the simple-string *format-control-string*)
				  :start *format-index*)))))


(defun format-newline (colon atsign parms)
  (when parms
    (format-error "Parameters not allowed"))
  (cond (colon
	 (when atsign (format-error "~:@<newline> is undefined")))
	(atsign (terpri)(format-eat-whitespace))
	(t (format-eat-whitespace))))


;;;; Pluralize word (~P) and Skip Arguments (~*)

(defun format-plural (colon atsign parms)
  (when parms
    (format-error "Parameters not allowed"))
  (when colon
    ;; Back up one argument first
    (let ((cdrs (- (length (the list *format-original-arguments*))
		   (length (the list *format-arguments*))
		   1)))
      (if (minusp cdrs)
	  (format-error  "No previous argument")
	  (setq *format-arguments*
		(nthcdr cdrs *format-original-arguments*)))))
  (if (eql (pop-format-arg) 1)
      (write-string (if atsign "y" ""))
      (write-string (if atsign "ies" "s"))))



;;; Skip arguments  (relative goto)  ~*

(defun format-skip-arguments (colon atsign parms)
  (cond (atsign
	 (with-format-parameters parms ((count 0))
	   (when (or (minusp count)
		     (> count (length *format-original-arguments*)))
	     (format-error "Illegal to go to non-existant argument"))
	   (setq *format-arguments*
		 (nthcdr count *format-original-arguments*))))
	(colon
	 (with-format-parameters parms ((count 1))
	   (let ((cdrs (- (length (the list *format-original-arguments*))
			  (length (the list *format-arguments*))
			  count)))
	     (if (minusp cdrs)
		 (format-error  "Skip to nonexistant argument")
		 (setq *format-arguments*
		       (nthcdr cdrs *format-original-arguments*))))))
	(t
	 (with-format-parameters parms ((count 1))
	   (if (> count (length *format-arguments*))
	       (format-error "Skip to nonexistant argument")
	       (setq *format-arguments* (nthcdr count *format-arguments*)))))))

  

;;;; Indirection  ~?

(defun format-indirection (colon atsign parms)
  (if (or colon parms) (format-error "Colon flag or parameters not allowed"))
  (let ((string (pop-format-arg)))
    (unless (stringp string)
      (format-error "Indirected control string is not a string"))
    (format-with-control-string string
      (if atsign
	  (sub-format 0 *format-length*)
	  (let* ((*format-original-arguments* (pop-format-arg))
		 (*format-arguments* *format-original-arguments*))
	    (unless (listp *format-arguments*)
	      (format-error "Argument must be a list"))
	    (sub-format 0 *format-length*))))))



;;; Tabulation  ~T

(defun format-tab (colon atsign parms)
  (with-format-parameters parms ((colnum 1) (colinc 1))
    (when colon
      (format-error "Tab-to in pixel units not supported"))
    (let* ((pos (stream-column *standard-output*))
	   (len (cond (pos (let ((col (if atsign (+ pos colnum) colnum)))
			     (if (> pos col)
				 (- colinc (rem (- pos col) colinc))
				 (- col pos))))
		      (atsign colnum)
		      (t 2))))
      (declare (fixnum len))
      (do ((i len (- i 40)))
	  ((<= i 40) (write-string "                                        "
				   *standard-output* :start 0 :end i))
	(declare (fixnum i))
	(write-string "                                        "
		      *standard-output*
		      :start 0
		      :end 40)))))

;;;; Ascii  ~A

(defun format-princ (colon atsign parms)
  (let ((arg (pop-format-arg)))
    (if (null parms)
	(if arg (princ arg) (write-string (if colon "()" "NIL")))
	(with-format-parameters parms
	   ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
	   (format-write-field (if arg
				   (princ-to-string arg)
				   (if colon "()" "NIL"))
			       mincol colinc minpad padchar atsign)))))



;;; S-expression  ~S
	    
(defun format-prin1 (colon atsign parms)
  (let ((arg (pop-format-arg)))
    (if (null parms)
	(if arg (prin1 arg) (write-string (if colon "()" "NIL")))
	(with-format-parameters parms
	   ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
	   (format-write-field (if arg
				   (prin1-to-string arg)
				   (if colon "()" "NIL"))
			       mincol colinc minpad padchar atsign)))))



;;; Character  ~C

(defun format-print-character (colon atsign parms)
  (with-format-parameters parms ()
    (let ((char (pop-format-arg)))
      (unless (characterp char)
	(format-error "Argument must be a character"))
      (cond ((not colon)
	     (cond (atsign
		    (prin1 char))
		   (t (write-char char))))
	    (t (format-print-named-character char t))))))

(defun format-print-named-character (char longp)
  (let* ((ch (code-char (char-code char)))	;strip funny bits
	 (name (char-name ch)))
    (cond (name (write-string (string-capitalize name)))
	  ;; Print control characters as "^"<char>
	  ((<= 0 (the fixnum (char-code char)) 31)
	   (write-char #\^)
	   (write-char (code-char (+ 64 (the fixnum (char-code char))))))
	  (t (write-char ch)))))
	  



;;;; NUMERIC PRINTING

;;; Insert commas after every third digit, scanning from right to left.

(defun format-add-commas (string commachar)
  (do* ((length (length (the string string)))
	(new-length (+ length
		       (the fixnum (floor (the fixnum (1- length)) 3))))
	(new-string (make-string new-length :initial-element commachar) 
		    (replace (the string new-string)
			     (the string string)
			     :start1 (max 0 (- new-pos 3))
			     :end1 new-pos
			     :start2 (max 0 (- pos 3))
			     :end2 pos))
	(pos length  (- pos 3))
	(new-pos new-length (- new-pos 4)))
       ((not (plusp pos)) new-string)
    (declare (fixnum length new-length pos new-pos))))


;;; Output a string in a field at MINCOL wide, padding with PADCHAR.
;;; Pads on the left if PADLEFT is true, else on the right.  If the
;;; length of the string plus the minimum permissible padding, MINPAD,
;;; is greater than MINCOL, the actual field size is rounded up to
;;; MINCOL + k * COLINC for the smallest possible positive integer k.

(defun format-write-field (string mincol colinc minpad padchar padleft)
  (unless (and (integerp mincol) (not (minusp mincol)))
    (format-error "Mincol must be a non-negative integer - ~S" mincol))
  (unless (and (integerp colinc) (plusp colinc))
    (format-error "Colinc must be a positive integer - ~S" colinc))
  (unless (and (integerp minpad) (not (minusp minpad)))
    (format-error "Minpad must be a non-negative integer - ~S" minpad))
  (unless (characterp padchar)
    (format-error "Padchar must be a character - ~S" padchar))
  (let* ((strlen (length (the string string)))
	 (width (format-round-columns (+ strlen minpad) mincol colinc)))
    (cond (padleft
	   (dotimes (i (- width strlen)) (write-char padchar))
	   (write-string string))
	  (t
	   (write-string string)
	   (dotimes (i (- width strlen)) (write-char padchar))))))


;;; FORMAT-PRINT-NUMBER does most of the work for the numeric printing
;;; directives.  The parameters are interpreted as defined for ~D.
;;;
(defun format-print-number (number radix print-commas-p print-sign-p parms)
  (with-format-parameters parms
    ((mincol 0) (padchar #\space) (commachar #\,))
    (let* ((*print-base* radix)
	   (text (princ-to-string number)))
      (if (integerp number)
	  (format-write-field
	   (if (and (plusp number) print-sign-p)
	       (if print-commas-p
		   (concatenate 'string "+" (format-add-commas text commachar))
		   (concatenate 'string "+" text))
	       (if print-commas-p
		   (format-add-commas text commachar)
		   text))
	   mincol 1 0 padchar t)	;colinc = 1, minpad = 0, padleft = t
	  (write-string text)))))


;;;; Print a cardinal number in English
(defun format-print-small-cardinal (n)
  (multiple-value-bind 
   (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref cardinal-ones hundreds))
      (write-string " hundred")
      (when (plusp rem) (write-char #\space)))    ; ; ; RAD
    (when (plusp rem)
      (multiple-value-bind (tens ones)
			   (truncate rem 10)
       (cond ((< 1 tens)
	      (write-string (svref cardinal-tens tens))
	      (when (plusp ones)
		(write-char #\-)
		(write-string (svref cardinal-ones ones))))
	     ((= tens 1)
	      (write-string (svref cardinal-teens ones)))
	     ((plusp ones)
	      (write-string (svref cardinal-ones ones))))))))

(defun format-print-cardinal (n)
  (cond ((minusp n)
	 (write-string "negative ")
	 (format-print-cardinal-aux (- n) 0 n))
	((zerop n)
	 (write-string "zero"))
	(t (format-print-cardinal-aux n 0 n))))

(defun format-print-cardinal-aux (n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (<= period 10)
      (format-error "Number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond) (write-char #\space))
      (format-print-small-cardinal here)
      (write-string (svref cardinal-periods period)))))


;;;; Print an ordinal number in English
(defun format-print-ordinal (n)
  (when (minusp n)
    (write-string "negative "))
  (let ((number (abs n)))
    (multiple-value-bind
     (top bot) (truncate number 100)
     (unless (zerop top) (format-print-cardinal (- number bot)))
     (when (and (plusp top) (plusp bot)) (write-char #\space))
     (multiple-value-bind
      (tens ones) (truncate bot 10)
      (cond ((= bot 12) (write-string "twelfth"))
	    ((= tens 1)
	     (write-string (svref cardinal-teens ones));;;RAD
	     (write-string "th"))
	    ((and (zerop tens) (plusp ones))
	     (write-string (svref ordinal-ones ones)))
	    ((and (zerop ones)(plusp tens))
	     (write-string (svref ordinal-tens tens)))
	    ((plusp bot)
	     (write-string (svref cardinal-tens tens))
	     (write-char #\-)
	     (write-string (svref ordinal-ones ones)))
	    ((plusp number) (write-string "th"))
	    (t (write-string "zeroeth")))))))


;;; Print Roman numerals

(defun format-print-old-roman (n)
  (unless (< 0 n 5000)
    (format-error "Number too large to print in old Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (start n (do ((i start (progn (write-char cur-char) (- i cur-val))))
		    ((< i cur-val) i))))
      ((zerop start))))


(defun format-print-roman (n)
  (unless (< 0 n 4000)
    (format-error "Number too large to print in Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (sub-chars '(#\C #\X #\X #\I #\I) (cdr sub-chars))
       (sub-val '(100 10 10 1 1 0) (cdr sub-val))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (cur-sub-char #\C (car sub-chars))
       (cur-sub-val 100 (car sub-val))
       (start n (do ((i start (progn (write-char cur-char) (- i cur-val))))
		    ((< i cur-val)
		     (cond ((<= (- cur-val cur-sub-val) i)
			    (write-char cur-sub-char)
			    (write-char cur-char)
			    (- i (- cur-val cur-sub-val)))
			   (t i))))))
	  ((zerop start))))



;;;; Format Radix Options (~D ~B ~O ~X ~R).

;;; Decimal  ~D

(defun format-print-decimal (colon atsign parms)
  (format-print-number (pop-format-arg) 10 colon atsign parms))


;;; Binary  ~B

(defun format-print-binary (colon atsign parms)
  (format-print-number (pop-format-arg) 2 colon atsign parms))


;;; Octal  ~O

(defun format-print-octal (colon atsign parms)
  (format-print-number (pop-format-arg) 8 colon atsign parms))


;;; Hexadecimal  ~X

(defun format-print-hexadecimal (colon atsign parms)
  (format-print-number (pop-format-arg) 16 colon atsign parms))


;;; Radix  ~R

(defun format-print-radix (colon atsign parms)
  (let ((number (pop-format-arg)))
    (if parms
	(format-print-number number (pop parms) colon atsign parms)
	(if atsign
	    (if colon
		(format-print-old-roman number)
		(format-print-roman number))
	    (if colon
		(format-print-ordinal number)
		(format-print-cardinal number))))))


;;;; FLOATING-POINT NUMBERS

;;; Fixed-format floating point  ~F
;;;
(defun format-fixed (colon atsign parms)
  (when colon
    (format-error "Colon flag not allowed"))
  (with-format-parameters parms
    ((w nil) (d nil) (k nil) (ovf nil) (pad #\space))
    ;;Note that the scale factor k defaults to nil.  This is interpreted as
    ;;zero by flonum-to-string, but more efficiently.
    (let ((number (pop-format-arg)))
      (if (floatp number)
	  (format-fixed-aux number w d k ovf pad atsign)
	  (if (rationalp number)
	      (format-fixed-aux
	       (coerce number 'short-float) w d k ovf pad atsign)
	      (let ((*print-base* 10))
		(format-write-field
		 (princ-to-string number) w 1 0 #\space t)))))))

(defun format-fixed-aux (number w d k ovf pad atsign)
  (if (not (or w d))
      (prin1 number)
      (let ((spaceleft w))
	(when (and w (or atsign (minusp number))) (decf spaceleft))
	(multiple-value-bind 
          (str len lpoint tpoint)
	  (flonum-to-string (abs number) spaceleft d k)
	  ;;if caller specifically requested no fraction digits, suppress the
	  ;;optional trailing zero
	  (when (and d (zerop d)) (setq tpoint nil))
	  (when w 
	    (decf spaceleft len)
	    ;;optional leading zero
	    (when lpoint
	      (if (or (> spaceleft 0) tpoint) ;force at least one digit
		  (decf spaceleft)
		  (setq lpoint nil)))
	    ;;optional trailing zero
	    (when tpoint
	      (if (> spaceleft 0)
		  (decf spaceleft)
		  (setq tpoint nil))))
	  (cond ((and w (< spaceleft 0) ovf)
		 ;;field width overflow
		 (dotimes (i w) (write-char ovf)))
		(t (when w (dotimes (i spaceleft) (write-char pad)))
		   (if (minusp number)
		       (write-char #\-)
		       (if atsign (write-char #\+)))
		   (when lpoint (write-char #\0))
		   (write-string str)
		   (when tpoint (write-char #\0))))))))


;;;; Exponential-format floating point  ~E


(defun format-exponential (colon atsign parms)
  (when colon
    (format-error "Colon flag not allowed"))
  (with-format-parameters parms
    ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (marker nil))
    (let ((number (pop-format-arg)))
      (if (floatp number)
	  (format-exp-aux number w d e k ovf pad marker atsign)
	  (if (rationalp number)
	      (format-exp-aux
	       (coerce number 'short-float) w d e k ovf pad marker atsign)
	      (let ((*print-base* 10))
		(format-write-field
		 (princ-to-string number) w 1 0 #\space t)))))))


(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\E
      (typecase number
	(short-float #\S)
;	(single-float #\F)
	(double-float #\D)
	(long-float #\L))))


;;;Here we prevent the scale factor from shifting all significance out of
;;;a number to the right.  We allow insignificant zeroes to be shifted in
;;;to the left right, athough it is an error to specify k and d such that this
;;;occurs.  Perhaps we should detect both these condtions and flag them as
;;;errors.  As for now, we let the user get away with it, and merely guarantee
;;;that at least one significant digit will appear.

(defun format-exp-aux (number w d e k ovf pad marker atsign)
  (if (not (or w d))
      (prin1 number)
      (multiple-value-bind (num expt)
	  (scale-exponent (abs number))
	(let* ((expt (- expt k))
	       (estr (princ-to-string (abs expt)))
	       (elen (if e (max (length estr) e) (length estr)))
	       (fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
	       (fmin (if (minusp k) (- 1 k) nil))
	       (spaceleft (if w (- w 2 elen) nil)))
	  (when (or atsign (minusp number)) (decf spaceleft))
	  (if (and w e ovf (> elen e))
	      ;;exponent overflow
	      (dotimes (i w) (write-char ovf))
	      (multiple-value-bind (fstr flen lpoint ) ;(tpoint)
		  (flonum-to-string num spaceleft fdig k fmin)
		(when w 
		  (decf spaceleft flen)
		  ;; (when tpoint (decf spaceleft)) ; deleted as per Rutgers' fix
		  (when lpoint
		    (if (> spaceleft 0)
			(decf spaceleft)
			(setq lpoint nil))))
		(cond ((and w (< spaceleft 0) ovf)
		       ;;significand overflow
		       (dotimes (i w) (write-char ovf)))
		      (t (when w
			   (dotimes (i spaceleft) (write-char pad)))
			 (if (minusp number)
			     (write-char #\-)
			     (if atsign (write-char #\+)))
			 (when lpoint (write-char #\0))
			 (write-string fstr)
			 ;; (when tpoint (write-char #\0)) ; as per Rutgers' fix
			 (write-char (if marker
					 marker
					 (format-exponent-marker number)))
			 (write-char (if (minusp expt) #\- #\+))
			 (when e 
			   ;;zero-fill before exponent if necessary
			   (dotimes (i (- e (length estr))) (write-char #\0)))
			 (write-string estr)))))))))



;;;; General Floating Point -  ~G

(defun format-general-float (colon atsign parms)
  (when colon
    (format-error "Colon flag not allowed"))
  (with-format-parameters parms
    ((w nil) (d nil) (e nil) (k nil) (ovf #\*) (pad #\space) (marker nil))
    (let ((number (pop-format-arg)))
      ;;The Excelsior edition does not say what to do if
      ;;the argument is not a float.  Here, we adopt the
      ;;conventions used by ~F and ~E.
      (if (floatp number)
	  (format-general-aux number w d e k ovf pad marker atsign)
	  (if (rationalp number)
	      (format-general-aux
	       (coerce number 'short-float) w d e k ovf pad marker atsign)
	      (let ((*print-base* 10))
		(format-write-field
		 (princ-to-string number) w 1 0 #\space t)))))))


(defun format-general-aux (number w d e k ovf pad marker atsign)
  (multiple-value-bind (ignore n) 
		       (scale-exponent (abs number))
    (declare (ignore ignore))
    ;;Default d if omitted.  The procedure is taken directly
    ;;from the definition given in the manual, and is not
    ;;very efficient, since we generate the digits twice.
    ;;Future maintainers are encouraged to improve on this.
    (unless d
      (multiple-value-bind (str len) 
			   (flonum-to-string (abs number))
	(declare (ignore str))
	(let ((q (if (= len 1) 1 (1- len))))
	  (setq d (max q (min n 7))))))
    (let* ((ee (if e (+ e 2) 4))
	   (ww (if w (- w ee) nil))
	   (dd (- d n)))
      (cond ((<= 0 dd d)
	     (format-fixed-aux number ww dd nil ovf pad atsign)
	     (dotimes (i ee) (write-char #\space)))
	    (t (format-exp-aux 
		 number w d e (or k 1) ovf pad marker atsign))))))


;;; Dollars floating-point format  ~$
(defun format-dollars (colon atsign parms)
  (with-format-parameters parms ((d 2) (n 1) (w 0) (pad #\space))
    (let ((number (pop-format-arg)))
      (if (rationalp number) (setq number (coerce number 'short-float)))
      (if (floatp number)
	  (let* ((signstr (if (minusp number) "-" (if atsign "+" "")))
		 (signlen (length signstr)))
	    (multiple-value-bind (str strlen ig2 ig3 pointplace)
				 (flonum-to-string number nil d nil)
	      (declare (ignore ig2 ig3))
	      (when colon (write-string signstr))
	      (dotimes (i (- w signlen (- n pointplace) strlen))
		(write-char pad))
	      (unless colon (write-string signstr))
	      (dotimes (i (- n pointplace)) (write-char #\0))
	      (write-string str)))
	  (let ((*print-base* 10))
	    (format-write-field (princ-to-string number) w 1 0 #\space t))))))

