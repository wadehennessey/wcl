;;; This file contains some of the system dependent code for CLX
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package "XLIB")

;;; The size of the output buffer.  Must be a multiple of 4.
(defparameter *output-buffer-size* 8192)

;;; Number of seconds to wait for a reply to a server request
(defparameter *reply-timeout* nil) 

#-(or clx-overlapping-arrays (not clx-little-endian))
(progn
  (defconstant *word-0* 0)
  (defconstant *word-1* 1)

  (defconstant *long-0* 0)
  (defconstant *long-1* 1)
  (defconstant *long-2* 2)
  (defconstant *long-3* 3))

#-(or clx-overlapping-arrays clx-little-endian)
(progn
  (defconstant *word-0* 1)
  (defconstant *word-1* 0)

  (defconstant *long-0* 3)
  (defconstant *long-1* 2)
  (defconstant *long-2* 1)
  (defconstant *long-3* 0))

;;; Set some compiler-options for often used code

(eval-when (eval compile load)

(defconstant *buffer-speed* 3
  "Speed compiler option for buffer code.")

(defconstant *buffer-safety* 0
  "Safety compiler option for buffer code.")

(defun declare-bufmac ()
  `(declare (optimize (speed ,*buffer-speed*) (safety ,*buffer-safety*))))

(defun declare-buffun ()
  `(declare (optimize (speed ,*buffer-speed*) (safety ,*buffer-safety*))))

)

(declaim (inline card8->int8 int8->card8
		 card16->int16 int16->card16
		 card32->int32 int32->card32))

(defun card8->int8 (x)
  (declare (type card8 x))
  (declare (values int8))
  #.(declare-buffun)
  (the int8 (if (logbitp 7 x)
		(the int8 (- x #x100))
	      x)))

(defun int8->card8 (x)
  (declare (type int8 x))
  (declare (values card8))
  #.(declare-buffun)
  (the card8 (ldb (byte 8 0) x)))

(defun card16->int16 (x)
  (declare (type card16 x))
  (declare (values int16))
  #.(declare-buffun)
  (the int16 (if (logbitp 15 x)
		 (the int16 (- x #x10000))
		 x)))

(defun int16->card16 (x)
  (declare (type int16 x))
  (declare (values card16))
  #.(declare-buffun)
  (the card16 (ldb (byte 16 0) x)))

(defun card32->int32 (x)
  (declare (type card32 x))
  (declare (values int32))
  #.(declare-buffun)
  (the int32 (if (logbitp 31 x)
		 (the int32 (- x #x100000000))
		 x)))

(defun int32->card32 (x)
  (declare (type int32 x))
  (declare (values card32))
  #.(declare-buffun)
  (the card32 (ldb (byte 32 0) x)))


(declaim (inline aref-card8 aset-card8 aref-int8 aset-int8))

(defun aref-card8 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (values card8))
  #.(declare-buffun)
  (the card8 (aref a i)))

(defun aset-card8 (v a i)
  (declare (type card8 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a i) v))

(defun aref-int8 (a i)
  (declare (type buffer-bytes a)
	   (type array-index i))
  (declare (values int8))
  #.(declare-buffun)
  (card8->int8 (aref a i)))

(defun aset-int8 (v a i)
  (declare (type int8 v)
	   (type buffer-bytes a)
	   (type array-index i))
  #.(declare-buffun)
  (setf (aref a i) (int8->card8 v)))

#+(or excl lcl3.0 clx-overlapping-arrays)
(declaim (inline aref-card16 aref-int16 aref-card32 aref-int32 aref-card29
		 aset-card16 aset-int16 aset-card32 aset-int32 aset-card29))

#+(and clx-overlapping-arrays (not Genera))
(progn

(defun aref-card16 (a i)
  (aref a i))

(defun aset-card16 (v a i)
  (setf (aref a i) v))

(defun aref-int16 (a i)
  (card16->int16 (aref a i)))

(defun aset-int16 (v a i)
  (setf (aref a i) (int16->card16 v))
  v)

(defun aref-card32 (a i)
  (aref a i))

(defun aset-card32 (v a i)
  (setf (aref a i) v))

(defun aref-int32 (a i)
  (card32->int32 (aref a i)))

(defun aset-int32 (v a i)
  (setf (aref a i) (int32->card32 v))
  v)

(defun aref-card29 (a i)
  (aref a i))

(defun aset-card29 (v a i)
  (setf (aref a i) v))

)

(declaim (inline aref-card16 aref-int16 aref-card32 aref-int32 aref-card29
		 aset-card16 aset-int16 aset-card32 aset-int32 aset-card29))

;;; New fast accessors
(defun aref-card16 (a i)
  (lisp::%unsigned-16bit-ref a (lisp::%ashr i 1)))

(defun aset-card16 (v a i)
  (lisp::%unsigned-16bit-def a (lisp::%ashr i 1) v)
  v)

(defun aref-int16 (a i)
  (lisp::%signed-16bit-ref a (lisp::%ashr i 1)))

(defun aset-int16 (v a i)
  (lisp::%signed-16bit-def a (lisp::%ashr i 1) v)
  v)

(defun aref-card32 (a i)
  (lisp::%unsigned-32bit-ref a (lisp::%ashr i 2)))

(defun aset-card32 (v a i)
  (lisp::%unsigned-32bit-def a (lisp::%ashr i 2) v)
  v)

(defun aref-int32 (a i)
  (lisp::%signed-32bit-ref a (lisp::%ashr i 2)))

(defun aset-int32 (v a i)
  (lisp::%signed-32bit-def a (lisp::%ashr i 2) v)
  v)

(defun aref-card29 (a i)
  (lisp::%unsigned-31bit-ref a (lisp::%ashr i 2)))

(defun aset-card29 (v a i)
  (lisp::%unsigned-31bit-def a (lisp::%ashr i 2) v)
  v)


#+really-slow
(progn
  (defun aref-card16 (a i)
    (declare (type buffer-bytes a)
	     (type array-index i))
    (declare (values card16))
    #.(declare-buffun)
    (the card16
	 (logior (the card16
		      (ash (the card8 (aref a (index+ i *word-1*))) 8))
		 (the card8
		      (aref a (index+ i *word-0*))))))

  (defun aset-card16 (v a i)
    (declare (type card16 v)
	     (type buffer-bytes a)
	     (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i *word-1*)) (the card8 (ldb (byte 8 8) v))
	  (aref a (index+ i *word-0*)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-int16 (a i)
    (declare (type buffer-bytes a)
	     (type array-index i))
    (declare (values int16))
    #.(declare-buffun)
    (the int16
	 (logior (the int16
		      (ash (the int8 (aref-int8 a (index+ i *word-1*))) 8))
		 (the card8
		      (aref a (index+ i *word-0*))))))

  (defun aset-int16 (v a i)
    (declare (type int16 v)
	     (type buffer-bytes a)
	     (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i *word-1*)) (the card8 (ldb (byte 8 8) v))
	  (aref a (index+ i *word-0*)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-card32 (a i)
    (declare (type buffer-bytes a)
	     (type array-index i))
    (declare (values card32))
    #.(declare-buffun)
    (the card32
	 (logior (the card32
		      (ash (the card8 (aref a (index+ i *long-3*))) 24))
		 (the card29
		      (ash (the card8 (aref a (index+ i *long-2*))) 16))
		 (the card16
		      (ash (the card8 (aref a (index+ i *long-1*))) 8))
		 (the card8
		      (aref a (index+ i *long-0*))))))

  (defun aset-card32 (v a i)
    (declare (type card32 v)
	     (type buffer-bytes a)
	     (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i *long-3*)) (the card8 (ldb (byte 8 24) v))
	  (aref a (index+ i *long-2*)) (the card8 (ldb (byte 8 16) v))
	  (aref a (index+ i *long-1*)) (the card8 (ldb (byte 8 8) v))
	  (aref a (index+ i *long-0*)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-int32 (a i)
    (declare (type buffer-bytes a)
	     (type array-index i))
    (declare (values int32))
    #.(declare-buffun)
    (the int32
	 (logior (the int32
		      (ash (the int8 (aref-int8 a (index+ i *long-3*))) 24))
		 (the card29
		      (ash (the card8 (aref a (index+ i *long-2*))) 16))
		 (the card16
		      (ash (the card8 (aref a (index+ i *long-1*))) 8))
		 (the card8
		      (aref a (index+ i *long-0*))))))

  (defun aset-int32 (v a i)
    (declare (type int32 v)
	     (type buffer-bytes a)
	     (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i *long-3*)) (the card8 (ldb (byte 8 24) v))
	  (aref a (index+ i *long-2*)) (the card8 (ldb (byte 8 16) v))
	  (aref a (index+ i *long-1*)) (the card8 (ldb (byte 8 8) v))
	  (aref a (index+ i *long-0*)) (the card8 (ldb (byte 8 0) v)))
    v)

  (defun aref-card29 (a i)
    (declare (type buffer-bytes a)
	     (type array-index i))
    (declare (values card29))
    #.(declare-buffun)
    (the card29
	 (logior (the card29
		      (ash (the card8 (aref a (index+ i *long-3*))) 24))
		 (the card29
		      (ash (the card8 (aref a (index+ i *long-2*))) 16))
		 (the card16
		      (ash (the card8 (aref a (index+ i *long-1*))) 8))
		 (the card8
		      (aref a (index+ i *long-0*))))))

  (defun aset-card29 (v a i)
    (declare (type card29 v)
	     (type buffer-bytes a)
	     (type array-index i))
    #.(declare-buffun)
    (setf (aref a (index+ i *long-3*)) (the card8 (ldb (byte 8 24) v))
	  (aref a (index+ i *long-2*)) (the card8 (ldb (byte 8 16) v))
	  (aref a (index+ i *long-1*)) (the card8 (ldb (byte 8 8) v))
	  (aref a (index+ i *long-0*)) (the card8 (ldb (byte 8 0) v)))
    v)
  )
(defsetf aref-card8 (a i) (v)
  `(aset-card8 ,v ,a ,i))

(defsetf aref-int8 (a i) (v)
  `(aset-int8 ,v ,a ,i))

(defsetf aref-card16 (a i) (v)
  `(aset-card16 ,v ,a ,i))

(defsetf aref-int16 (a i) (v)
  `(aset-int16 ,v ,a ,i))

(defsetf aref-card32 (a i) (v)
  `(aset-card32 ,v ,a ,i))

(defsetf aref-int32 (a i) (v)
  `(aset-int32 ,v ,a ,i))

(defsetf aref-card29 (a i) (v)
  `(aset-card29 ,v ,a ,i))

;;; Other random conversions

(defun rgb-val->card16 (value)
  ;; Short floats are good enough
  (declare (type rgb-val value))
  (declare (values card16))
  #.(declare-buffun)
  ;; Convert VALUE from float to card16
  (the card16 (values (round (the rgb-val value) #.(/ 1.0s0 #xffff)))))

(defun card16->rgb-val (value) 
  ;; Short floats are good enough
  (declare (type card16 value))
  (declare (values short-float))
  #.(declare-buffun)
  ;; Convert VALUE from card16 to float
  (the short-float (* (the card16 value) #.(/ 1.0s0 #xffff))))

(defun radians->int16 (value)
  ;; Short floats are good enough
  (declare (type angle value))
  (declare (values int16))
  #.(declare-buffun)
  (the int16 (values (round (the angle value) #.(float (/ pi 180.0s0 64.0s0) 0.0s0)))))

(defun int16->radians (value)
  ;; Short floats are good enough
  (declare (type int16 value))
  (declare (values short-float))
  #.(declare-buffun)
  (the float (* (the int16 value) (/ pi 180.0 64.0))))


;;-----------------------------------------------------------------------------
;; Character transformation
;;-----------------------------------------------------------------------------


;;; This stuff transforms chars to ascii codes in card8's and back.
;;; You might have to hack it a little to get it to work for your machine.

(declaim (inline char->card8 card8->char))

(macrolet ((char-translators ()
	     (let ((alist
		     `(#-lispm
		       ;; The normal ascii codes for the control characters.
		       ,@`((#\Return . 13)
			   (#\Linefeed . 10)
			   (#\Rubout . 127)
			   (#\Page . 12)
			   (#\Tab . 9)
			   (#\Backspace . 8)
			   (#\Newline . 10)
			   (#\Space . 32))
		       ;; One the lispm, #\Newline is #\Return, but we'd really like
		       ;; #\Newline to translate to ascii code 10, so we swap the
		       ;; Ascii codes for #\Return and #\Linefeed. We also provide
		       ;; mappings from the counterparts of these control characters
		       ;; so that the character mapping from the lisp machine
		       ;; character set to ascii is invertible.
		       #+lispm
		       ,@`((#\Return . 10)   (,(code-char  10) . ,(char-code #\Return))
			   (#\Linefeed . 13) (,(code-char  13) . ,(char-code #\Linefeed))
			   (#\Rubout . 127)  (,(code-char 127) . ,(char-code #\Rubout))
			   (#\Page . 12)     (,(code-char  12) . ,(char-code #\Page))
			   (#\Tab . 9)       (,(code-char   9) . ,(char-code #\Tab))
			   (#\Backspace . 8) (,(code-char   8) . ,(char-code #\Backspace))
			   (#\Newline . 10)  (,(code-char  10) . ,(char-code #\Newline))
			   (#\Space . 32)    (,(code-char  32) . ,(char-code #\Space)))
		       ;; The rest of the common lisp charater set with the normal
		       ;; ascii codes for them.
		       (#\! . 33) (#\" . 34) (#\# . 35) (#\$ . 36)
		       (#\% . 37) (#\& . 38) (#\' . 39) (#\( . 40)
		       (#\) . 41) (#\* . 42) (#\+ . 43) (#\, . 44)
		       (#\- . 45) (#\. . 46) (#\/ . 47) (#\0 . 48)
		       (#\1 . 49) (#\2 . 50) (#\3 . 51) (#\4 . 52)
		       (#\5 . 53) (#\6 . 54) (#\7 . 55) (#\8 . 56)
		       (#\9 . 57) (#\: . 58) (#\; . 59) (#\< . 60)
		       (#\= . 61) (#\> . 62) (#\? . 63) (#\@ . 64)
		       (#\A . 65) (#\B . 66) (#\C . 67) (#\D . 68)
		       (#\E . 69) (#\F . 70) (#\G . 71) (#\H . 72)
		       (#\I . 73) (#\J . 74) (#\K . 75) (#\L . 76)
		       (#\M . 77) (#\N . 78) (#\O . 79) (#\P . 80)
		       (#\Q . 81) (#\R . 82) (#\S . 83) (#\T . 84)
		       (#\U . 85) (#\V . 86) (#\W . 87) (#\X . 88)
		       (#\Y . 89) (#\Z . 90) (#\[ . 91) (#\\ . 92)
		       (#\] . 93) (#\^ . 94) (#\_ . 95) (#\` . 96)
		       (#\a . 97) (#\b . 98) (#\c . 99) (#\d . 100)
		       (#\e . 101) (#\f . 102) (#\g . 103) (#\h . 104)
		       (#\i . 105) (#\j . 106) (#\k . 107) (#\l . 108)
		       (#\m . 109) (#\n . 110) (#\o . 111) (#\p . 112)
		       (#\q . 113) (#\r . 114) (#\s . 115) (#\t . 116)
		       (#\u . 117) (#\v . 118) (#\w . 119) (#\x . 120)
		       (#\y . 121) (#\z . 122) (#\{ . 123) (#\| . 124)
		       (#\} . 125) (#\~ . 126))))
	       (cond ((dolist (pair alist nil)
			(when (not (= (char-code (car pair)) (cdr pair)))
			  (return t)))
		      `(progn
			 (defconstant *char-to-card8-translation-table*
				      ',(let ((array (make-array
						       (let ((max-char-code 255))
							 (dolist (pair alist)
							   (setq max-char-code
								 (max max-char-code
								      (char-code (car pair)))))
							 (1+ max-char-code))
						       :element-type 'card8)))
					  (dotimes (i (length array))
					    (setf (aref array i) (mod i 256)))
					  (dolist (pair alist)
					    (setf (aref array (char-code (car pair)))
						  (cdr pair)))
					  array))
			 (defconstant *card8-to-char-translation-table*
				      ',(let ((array (make-string 256)))
					  (dotimes (i (length array))
					    (setf (aref array i) (code-char i)))
					  (dolist (pair alist)
					    (setf (aref array (cdr pair)) (car pair)))
					  array))
			 #-Genera
			 (progn
  			   (defun char->card8 (char)
			     (declare (type base-char char))
			     #.(declare-buffun)
			     (the card8 (aref (the (simple-array card8 (*))
						   *char-to-card8-translation-table*)
					      (the array-index (char-code char)))))
			   (defun card8->char (card8)
			     (declare (type card8 card8))
			     #.(declare-buffun)
			     (the base-char
				  (aref (the simple-string *card8-to-char-translation-table*)
					card8)))
			   )
			 #+Genera
			 (progn
			   (defun char->card8 (char)
			     (declare lt:(side-effects reader reducible))
			     (aref *char-to-card8-translation-table* (char-code char)))
			   (defun card8->char (card8)
			     (declare lt:(side-effects reader reducible))
			     (aref *card8-to-char-translation-table* card8))
			   )
			 (dotimes (i 256)
			   (unless (= i (char->card8 (card8->char i)))
			     (warn "The card8->char mapping is not invertible through char->card8.  Info:~%~S"
				   (list i
					 (card8->char i)
					 (char->card8 (card8->char i))))
			     (return nil)))
			 (dotimes (i (length *char-to-card8-translation-table*))
			   (let ((char (code-char i)))
			     (unless (eql char (card8->char (char->card8 char)))
			       (warn "The char->card8 mapping is not invertible through card8->char.  Info:~%~S"
				     (list char
					   (char->card8 char)
					   (card8->char (char->card8 char))))
			       (return nil))))))
		     (t
		      `(progn
			 (defun char->card8 (char)
			   (declare (type base-char char))
			   #.(declare-buffun)
			   (the card8 (char-code char)))
			 (defun card8->char (card8)
			   (declare (type card8 card8))
			   #.(declare-buffun)
			   (the base-char (code-char card8)))
			 ))))))
  (char-translators))

;;-----------------------------------------------------------------------------
;; Process Locking
;;
;;	Common-Lisp doesn't provide process locking primitives, so we define
;;	our own here, based on Zetalisp primitives.  Holding-Lock is very
;;	similar to with-lock on The TI Explorer, and a little more efficient
;;	than with-process-lock on a Symbolics.
;;-----------------------------------------------------------------------------

;;; MAKE-PROCESS-LOCK: Creating a process lock.
(defun make-process-lock (name)
  (declare (ignore name))
  nil)

;;; HOLDING-LOCK: Execute a body of code with a lock held.

;;; The holding-lock macro takes a timeout keyword argument.  EVENT-LISTEN
;;; passes its timeout to the holding-lock macro, so any timeout you want to
;;; work for event-listen you should do for holding-lock.

;; If you're not sharing DISPLAY objects within a multi-processing
;; shared-memory environment, this is sufficient
(defmacro holding-lock ((locator display &optional whostate &key timeout) &body body)
  (declare (ignore locator display whostate timeout))
  `(progn ,@body))

;;; WITHOUT-ABORTS

;;; If you can inhibit asynchronous keyboard aborts inside the body of this
;;; macro, then it is a good idea to do this.  This macro is wrapped around
;;; request writing and reply reading to ensure that requests are atomically
;;; written and replies are atomically read from the stream.
(defmacro without-aborts (&body body)
  `(progn ,@body))

;;; PROCESS-BLOCK: Wait until a given predicate returns a non-NIL value.
;;; Caller guarantees that PROCESS-WAKEUP will be called after the predicate's
;;; value changes.

(defun process-block (whostate predicate &rest predicate-args)
  (declare (ignore whostate))
  (or (apply predicate predicate-args)
      (error "Program tried to wait with no scheduler.")))


;;; PROCESS-WAKEUP: Check some other process' wait function.

(declaim (inline process-wakeup))

(defun process-wakeup (process)
  (declare (ignore process))
  nil)


;;; CURRENT-PROCESS: Return the current process object for input locking and
;;; for calling PROCESS-WAKEUP.

(declaim (inline current-process))

;;; Default return NIL, which is acceptable even if there is a scheduler.
(defun current-process ()
  nil)


;;; WITHOUT-INTERRUPTS -- provide for atomic operations.
(defmacro without-interrupts (&body body)
  `(progn ,@body))

;;; CONDITIONAL-STORE:

;; This should use GET-SETF-METHOD to avoid evaluating subforms multiple times.
;; It doesn't because CLtL doesn't pass the environment to GET-SETF-METHOD.
(defmacro conditional-store (place old-value new-value)
  `(without-interrupts
     (cond ((eq ,place ,old-value)
	    (setf ,place ,new-value)
	    t))))

;;;----------------------------------------------------------------------------
;;; IO Error Recovery
;;;	All I/O operations are done within a WRAP-BUF-OUTPUT macro.
;;;	It prevents multiple mindless errors when the network craters.
;;;
;;;----------------------------------------------------------------------------

(defmacro wrap-buf-output ((buffer) &body body)
  ;; Error recovery wrapper
  `(unless (buffer-dead ,buffer)
     ,@body))

(defmacro wrap-buf-input ((buffer) &body body)
  (declare (ignore buffer))
  ;; Error recovery wrapper
  `(progn ,@body))


;;;----------------------------------------------------------------------------
;;; System dependent IO primitives
;;;	Functions for opening, reading writing forcing-output and closing 
;;;	the stream to the server.
;;;----------------------------------------------------------------------------

;;; OPEN-X-STREAM - create a stream for communicating to the appropriate X
;;; server

;;; Genera:

(defun open-x-stream (host display protocol)
  protocol;; unused
  (let ((fd (lisp::connect-to-server host display)))
    (when (minusp fd)
      (error "Failed to connect to server: ~A:~D, fd = ~D" 
	     host display fd))
    (lisp::open-user-stream :input-fd fd
			    :output-fd fd
			    :element-type '(unsigned-byte 8))))


;;; BUFFER-READ-DEFAULT - read data from the X stream
(defun buffer-read-default (display vector start end timeout)
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end)
	   (type (or null number) timeout))
  #.(declare-buffun)
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (or (cond ((null stream))
	      ((listen stream) nil)
	      ((eql timeout 0) :timeout)
	      ((buffer-input-wait-default display timeout)))
	(eq (read-vector stream vector start end nil :eof) :eof))))



;;; BUFFER-WRITE-DEFAULT - write data to the X stream
(defun buffer-write-default (vector display start end)
  (declare (type display display)
	   (type buffer-bytes vector)
	   (type array-index start end))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream) 
      (write-vector stream vector start end))))


;;; buffer-force-output-default - force output to the X stream
(defun buffer-force-output-default (display)
  ;; The default buffer force-output function for use with common-lisp streams
  (declare (type display display))
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (force-output stream))))

;;; BUFFER-CLOSE-DEFAULT - close the X stream
(defun buffer-close-default (display &key abort)
  ;; The default buffer close function for use with common-lisp streams
  (declare (type display display))
  #.(declare-buffun)
  (let ((stream (display-output-stream display)))
    (declare (type (or null stream) stream))
    (unless (null stream)
      (close stream :abort abort))))

;;; BUFFER-INPUT-WAIT-DEFAULT - wait for for input to be available for the
;;; buffer.  This is called in read-input between requests, so that a process
;;; waiting for input is abortable when between requests.  Should return
;;; :TIMEOUT if it times out, NIL otherwise.

;;; The default implementation
;; Poll for input every *buffer-read-polling-time* SECONDS.
(defparameter *buffer-read-polling-time* 0.5)

(defun buffer-input-wait-default (display timeout)
  (declare (type display display)
	   (type (or null number) timeout))
  (declare (values timeout))
  
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (cond ((null stream))
	  ((listen stream) nil)
	  ((eql timeout 0) :timeout)
	  ((not (null timeout))
	   (multiple-value-bind (npoll fraction)
	       (truncate timeout *buffer-read-polling-time*)
	     (dotimes (i npoll)			; Sleep for a time, then listen again
	       (sleep *buffer-read-polling-time*)
	       (when (listen stream)
		 (return-from buffer-input-wait-default nil)))
	     (when (plusp fraction)
	       (sleep fraction)			; Sleep a fraction of a second
	       (when (listen stream)		; and listen one last time
		 (return-from buffer-input-wait-default nil)))
	     :timeout)))))


;;; BUFFER-LISTEN-DEFAULT - returns T if there is input available for the
;;; buffer. This should never block, so it can be called from the scheduler.

;;; The default implementation is to just use listen.
(defun buffer-listen-default (display)
  (declare (type display display))
  (let ((stream (display-input-stream display)))
    (declare (type (or null stream) stream))
    (if (null stream)
	t
      (listen stream))))


;;;----------------------------------------------------------------------------
;;; System dependent speed hacks
;;;----------------------------------------------------------------------------

;;
;; WITH-STACK-LIST is used by WITH-STATE as a memory saving feature.
;; If your lisp doesn't have stack-lists, and you're worried about
;; consing garbage, you may want to re-write this to allocate and
;; initialize lists from a resource.
;;
(defmacro with-stack-list ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (MAPCAR #'EVAL '(exp1 ... expN)))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list ,@elements)))
     (declare (type cons ,var)
	      #+clx-ansi-common-lisp (dynamic-extent ,var))
     ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST* (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (APPLY #'LIST* (MAPCAR #'EVAL '(exp1 ... expN))))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list* ,@elements)))
     (declare (type cons ,var)
	      #+clx-ansi-common-lisp (dynamic-extent ,var))
     ,@body))

(declaim (inline buffer-replace))

;;; HEY! optimize 8bit replace
#+(and clx-overlapping-arrays (not (or lispm excl)))
(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type vector buf1 buf2)
	   (type array-index start1 end1 start2))
  (replace buf1 buf2 :start1 start1 :end1 end1 :start2 start2))

#-(or lispm lucid excl clx-overlapping-arrays)
(defun buffer-replace (buf1 buf2 start1 end1 &optional (start2 0))
  (declare (type buffer-bytes buf1 buf2)
	   (type array-index start1 end1 start2))
  (replace buf1 buf2 :start1 start1 :end1 end1 :start2 start2))


(defmacro with-gcontext-bindings ((gc saved-state indexes ts-index temp-mask temp-gc)
				  &body body)
  (let ((local-state (gensym))
	(resets nil))
    (dolist (index indexes)
      (push `(setf (svref ,local-state ,index) (svref ,saved-state ,index))
	    resets))
    `(unwind-protect
	 (progn
	   ,@body)
       (let ((,local-state (gcontext-local-state ,gc)))
	 (declare (type gcontext-state ,local-state))
	 ,@resets
	 (setf (svref ,local-state ,ts-index) 0))
       (when ,temp-gc
	 (restore-gcontext-temp-state ,gc ,temp-mask ,temp-gc))
       (deallocate-gcontext-state ,saved-state))))

;;;----------------------------------------------------------------------------
;;; How error detection should CLX do?
;;; Several levels are possible:
;;;
;;; 1. Do the equivalent of check-type on every argument.
;;; 
;;; 2. Simply report TYPE-ERROR.  This eliminates overhead of all the format
;;;    strings generated by check-type.
;;; 
;;; 3. Do error checking only on arguments that are likely to have errors
;;;    (like keyword names)
;;; 
;;; 4. Do error checking only where not doing so may dammage the envirnment
;;;    on a non-tagged machine (i.e. when storing into a structure that has
;;;    been passed in)
;;; 
;;; 5. No extra error detection code.  On lispm's, ASET may barf trying to
;;;    store a non-integer into a number array. 
;;; 
;;; How extensive should the error checking be?  For example, if the server
;;; expects a CARD16, is is sufficient for CLX to check for integer, or
;;; should it also check for non-negative and less than 65536?
;;;----------------------------------------------------------------------------
 
;; The *TYPE-CHECK?* constant controls how much error checking is done.
;; Possible values are:
;;    NIL      - Don't do any error checking
;;    t        - Do the equivalent of checktype on every argument
;;    :minimal - Do error checking only where errors are likely

;;; This controls macro expansion, and isn't changable at run-time You will
;;; probably want to set this to nil if you want good performance at
;;; production time.
(defconstant *type-check?* nil)

;; TYPE? is used to allow the code to do error checking at a different level from
;; the declarations.  It also does some optimizations for systems that don't have
;; good compiler support for TYPEP.  The definitions for CARD32, CARD16, INT16, etc.
;; include range checks.  You can modify TYPE? to do less extensive checking
;; for these types if you desire.

(defmacro type? (object type)
  (if (not (constantp type))
      `(typep ,object ,type)
    (progn
      (setq type (eval type))
      (let ((predicate (assoc type
			      '((drawable drawable-p) (window window-p)
				(pixmap pixmap-p) (cursor cursor-p)
				(font font-p) (gcontext gcontext-p)
				(colormap colormap-p) (null null)
				(integer integerp)))))
	(cond (predicate
	       `(,(second predicate) ,object))
	      ((eq type 'boolean)
	       't)			; Everything is a boolean.
	      (*type-check?*
	       `(locally (declare (optimize safety)) (typep ,object ',type)))
	      (t
	       `(typep ,object ',type)))))))

;; X-TYPE-ERROR is the function called for type errors.
;; If you want lots of checking, but are concerned about code size,
;; this can be made into a macro that ignores some parameters.
(defun x-type-error (object type &optional error-string)
  (error 'x-type-error
	 :datum object
	 :expected-type type
	 :type-string error-string))

(defun default-error-handler (display error-key &rest key-vals
				      &key asynchronous &allow-other-keys)
  (declare (type boolean asynchronous)
	   (dynamic-extent key-vals))
  ;; The default display-error-handler.
  ;; It signals the conditions listed in the DISPLAY file.
  (if asynchronous
      (apply #'cerror "Ignore" error-key :display display :error-key error-key key-vals)
      (apply #'error error-key :display display :error-key error-key key-vals)))

;;-----------------------------------------------------------------------------
;;  HOST hacking
;;-----------------------------------------------------------------------------

(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type (or stringable list) host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (values list))
  host family
  (error "HOST-ADDRESS not implemented yet."))


;;-----------------------------------------------------------------------------
;; Whether to use closures for requests or not.
;;-----------------------------------------------------------------------------

;;; If this macro expands to non-NIL, then request and locking code is
;;; compiled in a much more compact format, as the common code is shared, and
;;; the specific code is built into a closure that is funcalled by the shared
;;; code.  If your compiler makes efficient use of closures then you probably
;;; want to make this expand to T, as it makes the code more compact.

(defmacro use-closures () nil)

;;-----------------------------------------------------------------------------
;; Resource stuff
;;-----------------------------------------------------------------------------


;;; DEFAULT-RESOURCES-PATHNAME - The pathname of the resources file to load if
;;; a resource manager isn't running.

(defun default-resources-pathname ()
  (when #+unix t #-unix (search "Unix" (software-type) :test #'char-equal)
    (merge-pathnames (user-homedir-pathname) (pathname ".Xdefaults"))))


;;; RESOURCES-PATHNAME - The pathname of the resources file to load after the
;;; defaults have been loaded.

(defun resources-pathname ()
  (when #+unix t #-unix (search "Unix" (software-type) :test #'char-equal)
    (or #+(or excl (and lcl3.0 (not vax-vms)))
	(let ((string (#+excl sys:getenv
		       #+lcl3.0 lcl:environment-variable
		       "XENVIRONMENT")))
	  (when string
	    (pathname string)))
	(merge-pathnames
	  (user-homedir-pathname)
	  (pathname 
	    (concatenate 'simple-string ".Xdefaults-"
			 #+excl (short-site-name)
			 #-excl (machine-instance)))))))

(defun gc-cleanup ()
  (declare (special *event-free-list*
		    *pending-command-free-list*
		    *reply-buffer-free-lists*
		    *gcontext-local-state-cache*
		    *temp-gcontext-cache*))
  (setq *event-free-list* nil)
  (setq *pending-command-free-list* nil)
  (when (boundp '*reply-buffer-free-lists*)
    (fill *reply-buffer-free-lists* nil))
  (setq *gcontext-local-state-cache* nil)
  (setq *temp-gcontext-cache* nil)
  nil)


;;-----------------------------------------------------------------------------
;; WITH-STANDARD-IO-SYNTAX equivalent, used in (SETF WM-COMMAND)
;;-----------------------------------------------------------------------------

(defun with-standard-io-syntax-function (function)
  (declare #+lispm
	   (sys:downward-funarg function))
  (let ((*package* (find-package :user))
	(*print-array* t)
	(*print-base* 10)
	(*print-case* :upcase)
	(*print-circle* nil)
	(*print-escape* t)
	(*print-gensym* t)
	(*print-length* nil)
	(*print-level* nil)
	(*print-pretty* nil)
	(*print-radix* nil)
	(*read-base* 10)
	(*read-default-float-format* 'single-float)
	(*read-suppress* nil)
	(*print-structure* nil))
    (funcall function)))

#-(or clx-ansi-common-lisp Genera)
(defmacro with-standard-io-syntax (&body body)
  `(flet ((.with-standard-io-syntax-body. () ,@body))
     (with-standard-io-syntax-function #'.with-standard-io-syntax-body.)))


;;-----------------------------------------------------------------------------
;; DEFAULT-KEYSYM-TRANSLATE
;;-----------------------------------------------------------------------------

;;; If object is a character, char-bits are set from state.
;;;
;;; [the following isn't implemented (should it be?)]
;;; If object is a list, it is an alist with entries:
;;; (base-char [modifiers] [mask-modifiers])
;;; When MODIFIERS are specified, this character translation
;;; will only take effect when the specified modifiers are pressed.
;;; MASK-MODIFIERS can be used to specify a set of modifiers to ignore.
;;; When MASK-MODIFIERS is missing, all other modifiers are ignored.
;;; In ambiguous cases, the most specific translation is used.

(defun default-keysym-translate (display state object)
  (declare (type display display)
	   (type card16 state)
	   (type t object)
	   (values t)
	   (special left-meta-keysym right-meta-keysym
		    left-super-keysym right-super-keysym
		    left-hyper-keysym right-hyper-keysym))
  (when (characterp object)
    (when (logbitp (position :control *state-mask-vector*) state)
      (setf (char-bit object :control) 1))
    (when (or (state-keysymp display state left-meta-keysym)
	      (state-keysymp display state right-meta-keysym))
      (setf (char-bit object :meta) 1))
    (when (or (state-keysymp display state left-super-keysym)
	      (state-keysymp display state right-super-keysym))
      (setf (char-bit object :super) 1))
    (when (or (state-keysymp display state left-hyper-keysym)
	      (state-keysymp display state right-hyper-keysym))
      (setf (char-bit object :hyper) 1)))
  object)



;;-----------------------------------------------------------------------------
;; Image stuff
;;-----------------------------------------------------------------------------

;;; Types

(deftype pixarray-1-element-type ()
  'bit)

(deftype pixarray-4-element-type ()
  '(unsigned-byte 4))

(deftype pixarray-8-element-type ()
  '(unsigned-byte 8))

(deftype pixarray-16-element-type ()
  '(unsigned-byte 16))

(deftype pixarray-24-element-type ()
  '(unsigned-byte 24))

(deftype pixarray-32-element-type () '(unsigned-byte 32))

(deftype pixarray-1  ()
  '(array pixarray-1-element-type (* *)))

(deftype pixarray-4  ()
  '(array pixarray-4-element-type (* *)))

(deftype pixarray-8  ()
  '(array pixarray-8-element-type (* *)))

(deftype pixarray-16 ()
  '(array pixarray-16-element-type (* *)))

(deftype pixarray-24 ()
  '(array pixarray-24-element-type (* *)))

(deftype pixarray-32 ()
  '(array pixarray-32-element-type (* *)))

(deftype pixarray ()
  '(or pixarray-1 pixarray-4 pixarray-8 pixarray-16 pixarray-24 pixarray-32))

(deftype bitmap ()
  'pixarray-1)

;;; WITH-UNDERLYING-SIMPLE-VECTOR 

#+lcl3.0
(defmacro with-underlying-simple-vector
	  ((variable element-type pixarray) &body body)
  `(let ((,variable (sys:underlying-simple-vector ,pixarray)))
     (declare (type (simple-array ,element-type (*)) ,variable))
     ,@body))


;;; These are used to read and write pixels from and to CARD8s.
;;; READ-IMAGE-LOAD-BYTE is used to extract 1 and 4 bit pixels from CARD8s.

(defmacro read-image-load-byte (size position integer)
  (unless *image-bit-lsb-first-p* (setq position (- 7 position)))
  `(the (unsigned-byte ,size)
	(#-Genera ldb #+Genera sys:%logldb
	 (byte ,size ,position)
	 (the card8 ,integer))))

;;; READ-IMAGE-ASSEMBLE-BYTES is used to build 16, 24 and 32 bit pixels from
;;; the appropriate number of CARD8s.

(defmacro read-image-assemble-bytes (&rest bytes)
  (unless *image-byte-lsb-first-p* (setq bytes (reverse bytes)))
  (let ((it (first bytes))
	(count 0))
    (dolist (byte (rest bytes))
      (setq it
	    `(#-Genera dpb #+Genera sys:%logdpb 
	      (the card8 ,byte)
	      (byte 8 ,(incf count 8))
	      (the (unsigned-byte ,count) ,it))))
    `(the (unsigned-byte ,(* (length bytes) 8)) ,it)))

;;; WRITE-IMAGE-LOAD-BYTE is used to extract a CARD8 from a 16, 24 or 32 bit
;;; pixel.

(defmacro write-image-load-byte (position integer integer-size)
  integer-size
  (unless *image-byte-lsb-first-p* (setq position (- integer-size 8 position)))
  `(the card8
	(#-Genera ldb #+Genera sys:%logldb
	 (byte 8 ,position)
	 #-Genera (the (unsigned-byte ,integer-size) ,integer)
	 #+Genera ,integer
	 )))

;;; WRITE-IMAGE-ASSEMBLE-BYTES is used to build a CARD8 from 1 or 4 bit
;;; pixels.

(defmacro write-image-assemble-bytes (&rest bytes)
  (unless *image-bit-lsb-first-p* (setq bytes (reverse bytes)))
  (let ((size (floor 8 (length bytes)))
	(it (first bytes))
	(count 0))
    (dolist (byte (rest bytes))
      (setq it `(#-Genera dpb #+Genera sys:%logdpb
		 (the (unsigned-byte ,size) ,byte)
		 (byte ,size ,(incf count size))
		 (the (unsigned-byte ,count) ,it))))
    `(the card8 ,it)))

#+(or Genera lcl3.0 excl)
(defvar *computed-image-byte-lsb-first-p* *image-byte-lsb-first-p*)

#+(or Genera lcl3.0 excl)
(defvar *computed-image-bit-lsb-first-p* *image-bit-lsb-first-p*)

;;; The following table gives the bit ordering within bytes (when accessed
;;; sequentially) for a scanline containing 32 bits, with bits numbered 0 to
;;; 31, where bit 0 should be leftmost on the display.  For a given byte
;;; labelled A-B, A is for the most significant bit of the byte, and B is
;;; for the least significant bit.
;;; 
;;; legend:
;;; 	1   scanline-unit = 8
;;; 	2   scanline-unit = 16
;;; 	4   scanline-unit = 32
;;; 	M   byte-order = MostSignificant
;;; 	L   byte-order = LeastSignificant
;;; 	m   bit-order = MostSignificant
;;; 	l   bit-order = LeastSignificant
;;; 
;;; 
;;; format	ordering
;;; 
;;; 1Mm	00-07 08-15 16-23 24-31
;;; 2Mm	00-07 08-15 16-23 24-31
;;; 4Mm	00-07 08-15 16-23 24-31
;;; 1Ml	07-00 15-08 23-16 31-24
;;; 2Ml	15-08 07-00 31-24 23-16
;;; 4Ml	31-24 23-16 15-08 07-00
;;; 1Lm	00-07 08-15 16-23 24-31
;;; 2Lm	08-15 00-07 24-31 16-23
;;; 4Lm	24-31 16-23 08-15 00-07
;;; 1Ll	07-00 15-08 23-16 31-24
;;; 2Ll	07-00 15-08 23-16 31-24
;;; 4Ll	07-00 15-08 23-16 31-24

#+(or Genera lcl3.0 excl) 
(defconstant
  *image-bit-ordering-table*
  '(((1 (00 07) (08 15) (16 23) (24 31)) (nil nil))
    ((2 (00 07) (08 15) (16 23) (24 31)) (nil nil))
    ((4 (00 07) (08 15) (16 23) (24 31)) (nil nil))
    ((1 (07 00) (15 08) (23 16) (31 24)) (nil t))
    ((2 (15 08) (07 00) (31 24) (23 16)) (nil t))
    ((4 (31 24) (23 16) (15 08) (07 00)) (nil t))
    ((1 (00 07) (08 15) (16 23) (24 31)) (t   nil))
    ((2 (08 15) (00 07) (24 31) (16 23)) (t   nil))
    ((4 (24 31) (16 23) (08 15) (00 07)) (t   nil))
    ((1 (07 00) (15 08) (23 16) (31 24)) (t   t))
    ((2 (07 00) (15 08) (23 16) (31 24)) (t   t))
    ((4 (07 00) (15 08) (23 16) (31 24)) (t   t))))
  
#+(or Genera lcl3.0 excl) 
(defun compute-image-byte-and-bit-ordering ()
  (declare (values image-byte-lsb-first-p image-bit-lsb-first-p))
  ;; First compute the ordering 
  (let ((ordering nil)
	(a (make-array '(1 32) :element-type 'bit :initial-element 0)))
    (dotimes (i 4)
      (push (flet ((bitpos (a i n)
		     (declare (optimize (speed 3) (safety 0) (space 0)))
		     (declare (type (simple-array bit (* *)) a)
			      (type fixnum i n))
		     (with-underlying-simple-vector (v (unsigned-byte 8) a)
		       (prog2
			 (setf (aref v i) n)
			 (dotimes (i 32)
			   (unless (zerop (aref a 0 i))
			     (return i)))
			 (setf (aref v i) 0)))))
	      (list (bitpos a i #b10000000)
		    (bitpos a i #b00000001)))
	    ordering))
    (setq ordering (cons (floor *image-unit* 8) (nreverse ordering)))
    ;; Now from the ordering, compute byte-lsb-first-p and bit-lsb-first-p
    (let ((byte-and-bit-ordering
	    (second (assoc ordering *image-bit-ordering-table*
			   :test #'equal))))
      (unless byte-and-bit-ordering
	(error "Couldn't determine image byte and bit ordering~@
                measured image ordering = ~A"
	       ordering))
      (values-list byte-and-bit-ordering))))

#+(or Genera lcl3.0 excl) 
(multiple-value-setq
  (*computed-image-byte-lsb-first-p* *computed-image-bit-lsb-first-p*)
  (compute-image-byte-and-bit-ordering))

;;; If you can write fast routines that can read and write pixarrays out of a
;;; buffer-bytes, do it!  It makes the image code a lot faster.  The
;;; FAST-READ-PIXARRAY, FAST-WRITE-PIXARRAY and FAST-COPY-PIXARRAY routines
;;; return T if they can do it, NIL if they can't.

;;; FAST-READ-PIXARRAY - fill part of a pixarray from a buffer of card8s

#+(or lcl3.0 excl)
(defun fast-read-pixarray-1 (buffer-bbuf index array x y width height  
			     padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-1 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-1-element-type array)
      (do* ((start (index+ index
			   (index* y padded-bytes-per-line)
			   (index-ceiling x 8))
		   (index+ start padded-bytes-per-line))
	    (y 0 (index1+ y))
	    (left-bits (index-mod (index- x) 8))
	    (right-bits (index-mod (index- width left-bits) 8))
	    (middle-bits (index- width left-bits right-bits))
	    (middle-bytes (index-floor middle-bits 8)))
	   ((index>= y height))
	(declare (type array-index start y
		       left-bits right-bits middle-bits middle-bytes))
	(cond ((index< middle-bits 0)
	       (let ((byte (aref buffer-bbuf (index1- start)))
		     (x (array-row-major-index array y left-bits)))
		 (declare (type card8 byte)
			  (type array-index x))
		 (when (index> right-bits 6)
		   (setf (aref vector (index- x 1))
			 (read-image-load-byte 1 7 byte)))
		 (when (and (index> left-bits 1)
			    (index> right-bits 5))
		   (setf (aref vector (index- x 2))
			 (read-image-load-byte 1 6 byte)))
		 (when (and (index> left-bits 2)
			    (index> right-bits 4))
		   (setf (aref vector (index- x 3))
			 (read-image-load-byte 1 5 byte)))
		 (when (and (index> left-bits 3)
			    (index> right-bits 3))
		   (setf (aref vector (index- x 4))
			 (read-image-load-byte 1 4 byte)))
		 (when (and (index> left-bits 4)
			    (index> right-bits 2))
		   (setf (aref vector (index- x 5))
			 (read-image-load-byte 1 3 byte)))
		 (when (and (index> left-bits 5)
			    (index> right-bits 1))
		   (setf (aref vector (index- x 6))
			 (read-image-load-byte 1 2 byte)))
		 (when (index> left-bits 6)
		   (setf (aref vector (index- x 7))
			 (read-image-load-byte 1 1 byte)))))
	      (t
	       (unless (index-zerop left-bits)
		 (let ((byte (aref buffer-bbuf (index1- start)))
		       (x (array-row-major-index array y left-bits)))
		   (declare (type card8 byte)
			    (type array-index x))
		   (setf (aref vector (index- x 1))
			 (read-image-load-byte 1 7 byte))
		   (when (index> left-bits 1)
		     (setf (aref vector (index- x 2))
			   (read-image-load-byte 1 6 byte))
		     (when (index> left-bits 2)
		       (setf (aref vector (index- x 3))
			     (read-image-load-byte 1 5 byte))
		       (when (index> left-bits 3)
			 (setf (aref vector (index- x 4))
			       (read-image-load-byte 1 4 byte))
			 (when (index> left-bits 4)
			   (setf (aref vector (index- x 5))
				 (read-image-load-byte 1 3 byte))
			   (when (index> left-bits 5)
			     (setf (aref vector (index- x 6))
				   (read-image-load-byte 1 2 byte))
			     (when (index> left-bits 6)
			       (setf (aref vector (index- x 7))
				     (read-image-load-byte 1 1 byte))
			       ))))))))
	       (do* ((end (index+ start middle-bytes))
		     (i start (index1+ i))
		     (x (array-row-major-index array y left-bits) (index+ x 8)))
		    ((index>= i end)
		     (unless (index-zerop right-bits)
		       (let ((byte (aref buffer-bbuf end))
			     (x (array-row-major-index
				  array y (index+ left-bits middle-bits))))
			 (declare (type card8 byte)
				  (type array-index x))
			 (setf (aref vector (index+ x 0))
			       (read-image-load-byte 1 0 byte))
			 (when (index> right-bits 1)
			   (setf (aref vector (index+ x 1))
				 (read-image-load-byte 1 1 byte))
			   (when (index> right-bits 2)
			     (setf (aref vector (index+ x 2))
				   (read-image-load-byte 1 2 byte))
			     (when (index> right-bits 3)
			       (setf (aref vector (index+ x 3))
				     (read-image-load-byte 1 3 byte))
			       (when (index> right-bits 4)
				 (setf (aref vector (index+ x 4))
				       (read-image-load-byte 1 4 byte))
				 (when (index> right-bits 5)
				   (setf (aref vector (index+ x 5))
					 (read-image-load-byte 1 5 byte))
				   (when (index> right-bits 6)
				     (setf (aref vector (index+ x 6))
					   (read-image-load-byte 1 6 byte))
				     )))))))))
		 (declare (type array-index end i x))
		 (let ((byte (aref buffer-bbuf i)))
		   (declare (type card8 byte))
		   (setf (aref vector (index+ x 0))
			 (read-image-load-byte 1 0 byte))
		   (setf (aref vector (index+ x 1))
			 (read-image-load-byte 1 1 byte))
		   (setf (aref vector (index+ x 2))
			 (read-image-load-byte 1 2 byte))
		   (setf (aref vector (index+ x 3))
			 (read-image-load-byte 1 3 byte))
		   (setf (aref vector (index+ x 4))
			 (read-image-load-byte 1 4 byte))
		   (setf (aref vector (index+ x 5))
			 (read-image-load-byte 1 5 byte))
		   (setf (aref vector (index+ x 6))
			 (read-image-load-byte 1 6 byte))
		   (setf (aref vector (index+ x 7))
			 (read-image-load-byte 1 7 byte))))
	       )))))
  t)

#+(or lcl3.0 excl)
(defun fast-read-pixarray-4 (buffer-bbuf index array x y width height 
			     padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-4 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-4-element-type array)
      (do* ((start (index+ index
			   (index* y padded-bytes-per-line)
			   (index-ceiling x 2))
		   (index+ start padded-bytes-per-line))
	    (y 0 (index1+ y))
	    (left-nibbles (index-mod (index- x) 2))
	    (right-nibbles (index-mod (index- width left-nibbles) 2))
	    (middle-nibbles (index- width left-nibbles right-nibbles))
	    (middle-bytes (index-floor middle-nibbles 2)))
	   ((index>= y height))
	(declare (type array-index start y
		       left-nibbles right-nibbles middle-nibbles middle-bytes))
	(unless (index-zerop left-nibbles)
	  (setf (aref array y 0)
		(read-image-load-byte
		  4 4 (aref buffer-bbuf (index1- start)))))
	(do* ((end (index+ start middle-bytes))
	      (i start (index1+ i))
	      (x (array-row-major-index array y left-nibbles) (index+ x 2)))
	     ((index>= i end)
	      (unless (index-zerop right-nibbles)
		(setf (aref array y (index+ left-nibbles middle-nibbles))
		      (read-image-load-byte 4 0 (aref buffer-bbuf end)))))
	  (declare (type array-index end i x))
	  (let ((byte (aref buffer-bbuf i)))
	    (declare (type card8 byte))
	    (setf (aref vector (index+ x 0))
		  (read-image-load-byte 4 0 byte))
	    (setf (aref vector (index+ x 1))
		  (read-image-load-byte 4 4 byte))))
	)))
  t)

#+(or Genera lcl3.0 excl)
(defun fast-read-pixarray-24 (buffer-bbuf index array x y width height 
			      padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-24 array)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-24-element-type array)
      (do* ((start (index+ index
			   (index* y padded-bytes-per-line)
			   (index* x 3))
		   (index+ start padded-bytes-per-line))
	    (y 0 (index1+ y)))
	   ((index>= y height))
	(declare (type array-index start y))
	(do* ((end (index+ start (index* width 3)))
	      (i start (index+ i 3))
	      (x (array-row-major-index array y 0) (index1+ x)))
	     ((index>= i end))
	  (declare (type array-index end i x))
	  (setf (aref vector x)
		(read-image-assemble-bytes
		  (aref buffer-bbuf (index+ i 0))
		  (aref buffer-bbuf (index+ i 1))
		  (aref buffer-bbuf (index+ i 2))))))))
  t)

#+lispm
(defun fast-read-pixarray-using-bitblt
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel)
  (#+Genera sys:stack-let* #-Genera let*
   ((dimensions (list (+ y height)
		      (floor (* padded-bytes-per-line 8) bits-per-pixel)))
    (a (make-array
	 dimensions
	 :element-type (array-element-type pixarray)
	 :displaced-to bbuf
	 :displaced-index-offset (floor (* boffset 8) bits-per-pixel))))
   (sys:bitblt boole-1 width height a x y pixarray 0 0))
  t)

#+(or Genera lcl3.0 excl)
(defun fast-read-pixarray-with-swap
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type array-index boffset
		 padded-bytes-per-line)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type boolean byte-lsb-first-p bit-lsb-first-p))
  (unless (index= bits-per-pixel 24)
    (let ((pixarray-padded-bits-per-line
	    (if (index= height 1) 0
	      (index* (index- (array-row-major-index pixarray 1 0)
			      (array-row-major-index pixarray 0 0))
		      bits-per-pixel)))
	  (x-bits (index* x bits-per-pixel)))
      (declare (type array-index pixarray-padded-bits-per-line x-bits))
      (when (if (eq *computed-image-byte-lsb-first-p* *computed-image-bit-lsb-first-p*)
		(and (index-zerop (index-mod pixarray-padded-bits-per-line 8))
		     (index-zerop (index-mod x-bits 8)))
	      (and (index-zerop (index-mod pixarray-padded-bits-per-line *image-unit*))
		   (index-zerop (index-mod x-bits *image-unit*))))
	(multiple-value-bind (image-swap-function image-swap-lsb-first-p)
	    (image-swap-function
	      bits-per-pixel 
	      unit byte-lsb-first-p bit-lsb-first-p
	      *image-unit* *computed-image-byte-lsb-first-p*
	      *computed-image-bit-lsb-first-p*)
	  (declare (type symbol image-swap-function)
		   (type boolean image-swap-lsb-first-p))
	  (with-underlying-simple-vector (dst card8 pixarray)
	    (funcall
	      (symbol-function image-swap-function) bbuf dst
	      (index+ boffset
		      (index* y padded-bytes-per-line)
		      (index-floor x-bits 8))
	      0 (index-ceiling (index* width bits-per-pixel) 8)
	      padded-bytes-per-line
	      (index-floor pixarray-padded-bits-per-line 8)
	      height image-swap-lsb-first-p)))
	t))))

(defun fast-read-pixarray (bbuf boffset pixarray
			   x y width height padded-bytes-per-line
			   bits-per-pixel
			   unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type array-index boffset
		 padded-bytes-per-line)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type boolean byte-lsb-first-p bit-lsb-first-p))
  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
	 bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (or
    #+(or Genera lcl3.0 excl)
    (fast-read-pixarray-with-swap
      bbuf boffset pixarray x y width height padded-bytes-per-line
      bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
    (let ((function
	    (or #+lispm
		(and (= (sys:array-element-size pixarray) bits-per-pixel)
		     (zerop (index-mod padded-bytes-per-line 4))
		     (zerop (index-mod
			      (* #+Genera (sys:array-row-span pixarray)
				 #-Genera (array-dimension pixarray 1)
				 bits-per-pixel)
			      32))
		     #'fast-read-pixarray-using-bitblt)
		#+(or lcl3.0 excl)
		(and (index= bits-per-pixel 1)
		     #'fast-read-pixarray-1)
		#+(or lcl3.0 excl)
		(and (index= bits-per-pixel 4)
		     #'fast-read-pixarray-4)
		#+(or Genera lcl3.0 excl)
		(and (index= bits-per-pixel 24)
		     #'fast-read-pixarray-24))))
      (when function
	(read-pixarray-internal
	  bbuf boffset pixarray x y width height padded-bytes-per-line
	  bits-per-pixel function
	  unit byte-lsb-first-p bit-lsb-first-p
	  *image-unit* *image-byte-lsb-first-p* *image-bit-lsb-first-p*)))))

;;; FAST-WRITE-PIXARRAY - copy part of a pixarray into an array of CARD8s

#+(or lcl3.0 excl)
(defun fast-write-pixarray-1 (buffer-bbuf index array x y width height
			      padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-1 array)
	   (type card16 x y width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-1-element-type array)
      (do* ((h 0 (index1+ h))
	    (y y (index1+ y))
	    (right-bits (index-mod width 8))
	    (middle-bits (index- width right-bits))
	    (middle-bytes (index-ceiling middle-bits 8))
	    (start index (index+ start padded-bytes-per-line)))
	   ((index>= h height))
	(declare (type array-index h y right-bits middle-bits
		       middle-bytes start))
	(do* ((end (index+ start middle-bytes))
	      (i start (index1+ i))
	      (start-x x)
	      (x (array-row-major-index array y start-x) (index+ x 8)))
	     ((index>= i end)
	      (unless (index-zerop right-bits)
		(let ((x (array-row-major-index
			   array y (index+ start-x middle-bits))))
		  (declare (type array-index x))
		  (setf (aref buffer-bbuf end)
			(write-image-assemble-bytes
			  (aref vector (index+ x 0))
			  (if (index> right-bits 1)
			      (aref vector (index+ x 1))
			    0)
			  (if (index> right-bits 2)
			      (aref vector (index+ x 2))
			    0)
			  (if (index> right-bits 3)
			      (aref vector (index+ x 3))
			    0)
			  (if (index> right-bits 4)
			      (aref vector (index+ x 4))
			    0)
			  (if (index> right-bits 5)
			      (aref vector (index+ x 5))
			    0)
			  (if (index> right-bits 6)
			      (aref vector (index+ x 6))
			    0)
			  0)))))
	  (declare (type array-index end i start-x x))
	  (setf (aref buffer-bbuf i)
		(write-image-assemble-bytes
		  (aref vector (index+ x 0))
		  (aref vector (index+ x 1))
		  (aref vector (index+ x 2))
		  (aref vector (index+ x 3))
		  (aref vector (index+ x 4))
		  (aref vector (index+ x 5))
		  (aref vector (index+ x 6))
		  (aref vector (index+ x 7))))))))
  t)

#+(or lcl3.0 excl)
(defun fast-write-pixarray-4 (buffer-bbuf index array x y width height
			      padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-4 array)
	   (type int16 x y)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-4-element-type array)
      (do* ((h 0 (index1+ h))
	    (y y (index1+ y))
	    (right-nibbles (index-mod width 2))
	    (middle-nibbles (index- width right-nibbles))
	    (middle-bytes (index-ceiling middle-nibbles 2))
	    (start index (index+ start padded-bytes-per-line)))
	   ((index>= h height))
	(declare (type array-index h y right-nibbles middle-nibbles
		       middle-bytes start))
	(do* ((end (index+ start middle-bytes))
	      (i start (index1+ i))
	      (start-x x)
	      (x (array-row-major-index array y start-x) (index+ x 2)))
	     ((index>= i end)
	      (unless (index-zerop right-nibbles)
		(setf (aref buffer-bbuf end)
		      (write-image-assemble-bytes
			(aref array y (index+ start-x middle-nibbles))
			0))))
	  (declare (type array-index end i start-x x))
	  (setf (aref buffer-bbuf i)
		(write-image-assemble-bytes
		  (aref vector (index+ x 0))
		  (aref vector (index+ x 1))))))))
  t)

#+(or Genera lcl3.0 excl)
(defun fast-write-pixarray-24 (buffer-bbuf index array x y width height
			       padded-bytes-per-line bits-per-pixel)
  (declare (type buffer-bytes buffer-bbuf)
	   (type pixarray-24 array)
	   (type int16 x y)
	   (type card16 width height)
	   (type array-index index padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (ignore bits-per-pixel))
  #.(declare-buffun)
  (with-vector (buffer-bbuf buffer-bytes)
    (with-underlying-simple-vector (vector pixarray-24-element-type array)
      (do* ((h 0 (index1+ h))
	    (y y (index1+ y))
	    (start index (index+ start padded-bytes-per-line)))
	   ((index>= h height))
	(declare (type array-index y start))
	(do* ((end (index+ start (index* width 3)))
	      (i start (index+ i 3))
	      (x (array-row-major-index array y x) (index1+ x)))
	     ((index>= i end))
	  (declare (type array-index end i x))
	  (let ((pixel (aref vector x)))
	    (declare (type pixarray-24-element-type pixel))
	    (setf (aref buffer-bbuf (index+ i 0))
		  (write-image-load-byte 0 pixel 24))
	    (setf (aref buffer-bbuf (index+ i 1))
		  (write-image-load-byte 8 pixel 24))
	    (setf (aref buffer-bbuf (index+ i 2))
		  (write-image-load-byte 16 pixel 24)))))))
  t)

#+lispm
(defun fast-write-pixarray-using-bitblt
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel)
  (#+Genera sys:stack-let* #-Genera let*
   ((dimensions (list (+ y height)
		      (floor (* padded-bytes-per-line 8) bits-per-pixel)))
    (a (make-array
	 dimensions
	 :element-type (array-element-type pixarray)
	 :displaced-to bbuf
	 :displaced-index-offset (floor (* boffset 8) bits-per-pixel))))
   (sys:bitblt boole-1 width height pixarray x y a 0 0))
  t)

#+(or Genera lcl3.0 excl)
(defun fast-write-pixarray-with-swap
       (bbuf boffset pixarray x y width height padded-bytes-per-line
	bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type array-index boffset padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type boolean byte-lsb-first-p bit-lsb-first-p))
  (unless (index= bits-per-pixel 24)
    (let ((pixarray-padded-bits-per-line
	    (if (index= height 1) 0
	      (index* (index- (array-row-major-index pixarray 1 0)
			      (array-row-major-index pixarray 0 0))
		      bits-per-pixel)))
	  (pixarray-start-bit-offset
	    (index* (array-row-major-index pixarray y x)
		    bits-per-pixel)))
      (declare (type array-index pixarray-padded-bits-per-line
		     pixarray-start-bit-offset))
      (when (if (eq *computed-image-byte-lsb-first-p* *computed-image-bit-lsb-first-p*)
		(and (index-zerop (index-mod pixarray-padded-bits-per-line 8))
		     (index-zerop (index-mod pixarray-start-bit-offset 8)))
	      (and (index-zerop (index-mod pixarray-padded-bits-per-line *image-unit*))
		   (index-zerop (index-mod pixarray-start-bit-offset *image-unit*))))
	(multiple-value-bind (image-swap-function image-swap-lsb-first-p)
	    (image-swap-function
	      bits-per-pixel
	      *image-unit* *computed-image-byte-lsb-first-p*
	      *computed-image-bit-lsb-first-p*
	      unit byte-lsb-first-p bit-lsb-first-p)
	  (declare (type symbol image-swap-function)
		   (type boolean image-swap-lsb-first-p))
	  (with-underlying-simple-vector (src card8 pixarray)
	    (funcall
	      (symbol-function image-swap-function)
	      src bbuf (index-floor pixarray-start-bit-offset 8) boffset
	      (index-ceiling (index* width bits-per-pixel) 8)
	      (index-floor pixarray-padded-bits-per-line 8)
	      padded-bytes-per-line height image-swap-lsb-first-p))
	  t)))))

(defun fast-write-pixarray (bbuf boffset pixarray x y width height
			    padded-bytes-per-line bits-per-pixel
			    unit byte-lsb-first-p bit-lsb-first-p)
  (declare (type buffer-bytes bbuf)
	   (type pixarray pixarray)
	   (type card16 x y width height)
	   (type array-index boffset padded-bytes-per-line)
	   (type (member 1 4 8 16 24 32) bits-per-pixel)
	   (type (member 8 16 32) unit)
	   (type boolean byte-lsb-first-p bit-lsb-first-p))
  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
	 bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
  (or
    #+(or Genera lcl3.0 excl)
    (fast-write-pixarray-with-swap
      bbuf boffset pixarray x y width height padded-bytes-per-line
      bits-per-pixel unit byte-lsb-first-p bit-lsb-first-p)
    (let ((function
	    (or #+lispm
		(and (= (sys:array-element-size pixarray) bits-per-pixel)
		     (zerop (index-mod padded-bytes-per-line 4))
		     (zerop (index-mod
			      (* #+Genera (sys:array-row-span pixarray)
				 #-Genera (array-dimension pixarray 1)
				 bits-per-pixel)
			      32))
		     #'fast-write-pixarray-using-bitblt)
		#+(or lcl3.0 excl)
		(and (index= bits-per-pixel 1)
		     #'fast-write-pixarray-1)
		#+(or lcl3.0 excl)
		(and (index= bits-per-pixel 4)
		     #'fast-write-pixarray-4)
		#+(or Genera lcl3.0 excl)
		(and (index= bits-per-pixel 24)
		     #'fast-write-pixarray-24))))
      (when function
	(write-pixarray-internal
	  bbuf boffset pixarray x y width height padded-bytes-per-line
	  bits-per-pixel function
	  *image-unit* *image-byte-lsb-first-p* *image-bit-lsb-first-p*
	  unit byte-lsb-first-p bit-lsb-first-p)))))

;;; FAST-COPY-PIXARRAY - copy part of a pixarray into another

(defun fast-copy-pixarray (pixarray copy x y width height bits-per-pixel)
  (declare (type pixarray pixarray copy)
	   (type card16 x y width height)
	   (type (member 1 4 8 16 24 32) bits-per-pixel))
  (progn pixarray copy x y width height bits-per-pixel nil)
  (or
    #+lispm
    (let* ((pixarray-padded-pixels-per-line
	     #+Genera (sys:array-row-span pixarray)
	     #-Genera (array-dimension pixarray 1))
	   (pixarray-padded-bits-per-line
	     (* pixarray-padded-pixels-per-line bits-per-pixel))
	   (copy-padded-pixels-per-line
	     #+Genera (sys:array-row-span copy)
	     #-Genera (array-dimension copy 1))
	   (copy-padded-bits-per-line
	     (* copy-padded-pixels-per-line bits-per-pixel)))
      (when (and (= (sys:array-element-size pixarray) bits-per-pixel)
		 (zerop (index-mod pixarray-padded-bits-per-line 32))
		 (zerop (index-mod copy-padded-bits-per-line 32)))
	(sys:bitblt boole-1 width height pixarray x y copy 0 0)
	t))
    #+(or lcl3.0 excl)
    (unless (index= bits-per-pixel 24)
      (let ((pixarray-padded-bits-per-line
	      (if (index= height 1) 0
		(index* (index- (array-row-major-index pixarray 1 0)
				(array-row-major-index pixarray 0 0))
			bits-per-pixel)))
	    (copy-padded-bits-per-line
	      (if (index= height 1) 0
		(index* (index- (array-row-major-index copy 1 0)
				(array-row-major-index copy 0 0))
			bits-per-pixel)))
	    (pixarray-start-bit-offset
	      (index* (array-row-major-index pixarray y x)
		      bits-per-pixel)))
	(declare (type array-index pixarray-padded-bits-per-line
		       copy-padded-bits-per-line pixarray-start-bit-offset))
	(when (if (eq *computed-image-byte-lsb-first-p* *computed-image-bit-lsb-first-p*)
		  (and (index-zerop (index-mod pixarray-padded-bits-per-line 8))
		       (index-zerop (index-mod copy-padded-bits-per-line 8))
		       (index-zerop (index-mod pixarray-start-bit-offset 8)))
		(and (index-zerop (index-mod pixarray-padded-bits-per-line *image-unit*))
		     (index-zerop (index-mod copy-padded-bits-per-line *image-unit*))
		     (index-zerop (index-mod pixarray-start-bit-offset *image-unit*))))
	  (with-underlying-simple-vector (src card8 pixarray)
	    (with-underlying-simple-vector (dst card8 copy)
	      (image-noswap
		src dst
		(index-floor pixarray-start-bit-offset 8) 0
		(index-ceiling (index* width bits-per-pixel) 8)
		(index-floor pixarray-padded-bits-per-line 8)
		(index-floor copy-padded-bits-per-line 8)
		height nil)))
	  t)))
    #+(or lcl3.0 excl)
    (macrolet
      ((copy (type element-type)
	 `(let ((pixarray pixarray)
		(copy copy))
	    (declare (type ,type pixarray copy))
	    #.(declare-buffun)
	    (with-underlying-simple-vector (src ,element-type pixarray)
	      (with-underlying-simple-vector (dst ,element-type copy)
		(do* ((dst-y 0 (index1+ dst-y))
		      (src-y y (index1+ src-y)))
		     ((index>= dst-y height))
		  (declare (type card16 dst-y src-y))
		  (do* ((dst-idx (array-row-major-index copy dst-y 0)
				 (index1+ dst-idx))
			(dst-end (index+ dst-idx width))
			(src-idx (array-row-major-index pixarray src-y x)
				 (index1+ src-idx)))
		       ((index>= dst-idx dst-end))
		    (declare (type array-index dst-idx src-idx dst-end))
		    (setf (aref dst dst-idx)
			  (the ,element-type (aref src src-idx))))))))))
      (ecase bits-per-pixel
	(1  (copy pixarray-1  pixarray-1-element-type))
	(4  (copy pixarray-4  pixarray-4-element-type))
	(8  (copy pixarray-8  pixarray-8-element-type))
	(16 (copy pixarray-16 pixarray-16-element-type))
	(24 (copy pixarray-24 pixarray-24-element-type))
	(32 (copy pixarray-32 pixarray-32-element-type)))
      t)))
