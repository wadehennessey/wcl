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

;;; A bug in GCC 2.1 prevents using -O when compiling input.lisp.
;;; [in READ-INPUT nil assign from SHIFTF is ignored, events are lost]
;;; Only use sun cc to compile, or manually compile input.c without -O.

(in-package "XLIB")

;;;
;;; Error Handling

(eval-when (eval compile load)
  (defparameter
      *xerror-vector*
    '#(unknown-error
       request-error			; 1  bad request code
       value-error			; 2  integer parameter out of range
       window-error			; 3  parameter not a Window
       pixmap-error			; 4  parameter not a Pixmap
       atom-error			; 5  parameter not an Atom
       cursor-error			; 6  parameter not a Cursor
       font-error			; 7  parameter not a Font
       match-error			; 8  parameter mismatch
       drawable-error			; 9  parameter not a Pixmap or Window
       access-error			; 10 attempt to access private resource
       alloc-error			; 11 insufficient resources
       colormap-error			; 12 no such colormap
       gcontext-error			; 13 parameter not a GContext
       id-choice-error			; 14 invalid resource ID for this connection
       name-error			; 15 font or color name does not exist
       length-error			; 16 request length incorrect;
					;    internal Xlib error
       implementation-error		; 17 server is defective
       ))
  )

(defun make-error (display event asynchronous)
  (declare (type display display)
	   (type reply-buffer event)
	   (type boolean asynchronous))
  (reading-event (event)
    (let* ((error-code (read-card8 1))
	   (error-key (get-error-key display error-code))
	   (error-decode-function (get error-key 'error-decode-function))
	   (params (funcall error-decode-function display event)))
      (list* error-code error-key
	     :asynchronous asynchronous :current-sequence (display-request-number display)
	     params))))

(defun report-error (display error-code error-key &rest params)
  (declare (type display display)
	   (dynamic-extent params))
  ;; All errors (synchronous and asynchronous) are processed by calling
  ;; an error handler in the display.  The handler is called with the display
  ;; as the first argument and the error-key as its second argument. If handler is
  ;; an array it is expected to contain handler functions specific to
  ;; each error; the error code is used to index the array, fetching the
  ;; appropriate handler. Any results returned by the handler are ignored;;
  ;; it is assumed the handler either takes care of the error completely,
  ;; or else signals. For all core errors, additional keyword/value argument
  ;; pairs are:
  ;;    :major integer
  ;;    :minor integer
  ;;    :sequence integer
  ;;    :current-sequence integer
  ;;    :asynchronous (member t nil)
  ;; For :colormap, :cursor, :drawable, :font, :GContext, :id-choice, :pixmap, and :window
  ;; errors another pair is:
  ;;    :resource-id integer
  ;; For :atom errors, another pair is:
  ;;    :atom-id integer
  ;; For :value errors, another pair is:
  ;;    :value integer
  (let* ((handler (display-error-handler display))
	 (handler-function
	   (if (type? handler 'sequence)
	       (elt handler error-code)
	     handler)))
    (apply handler-function display error-key params)))

(defun request-name (code &optional display)
  (if (< code (length *request-names*))
      (svref *request-names* code)
      (dolist (extension (and display (display-extension-alist display))
	       "unknown")
	(when (= code (second extension))
	  (return (first extension))))))

(defun report-request-error (condition stream)
  (let ((error-key (request-error-error-key condition))
	(asynchronous (request-error-asynchronous condition))
	(major (request-error-major condition))
	(minor (request-error-minor condition))
	(sequence (request-error-sequence condition))
	(current-sequence (request-error-current-sequence condition)))		   
    (format stream "~:[~;Asynchronous ~]~a in ~:[request ~d (last request was ~d) ~;current request~2* ~] Code ~d.~d [~a]"
	    asynchronous error-key (= sequence current-sequence)
	    sequence current-sequence major minor
	    (request-name major (request-error-display condition)))))

(define-condition request-error (error)
  ((display :reader request-error-display :initarg :display)
   (error-key :reader request-error-error-key :initarg :error-key)
   (major :reader request-error-major :initarg :major)
   (minor :reader request-error-minor :initarg :minor)
   (sequence :reader request-error-sequence :initarg :sequence)
   (current-sequence :reader request-error-current-sequence :initarg :current-sequence)
   (asynchronous :reader request-error-asynchronous :initarg :asynchronous))
  (:report report-request-error))

(define-condition resource-error (request-error)
  ((resource-id :reader resource-error-resource-id :initarg :resource-id))
  (:report
    (lambda (condition stream)
      (report-request-error condition stream)
      (format stream " ID #x~x" (resource-error-resource-id condition)))))  

(define-condition unknown-error (request-error)
  ((error-code :reader unknown-error-error-code :initarg :error-code))
  (:report
    (lambda (condition stream)
      (report-request-error condition stream)
      (format stream " Error Code ~d." (unknown-error-error-code condition)))))

(define-condition access-error (request-error) ())

(define-condition alloc-error (request-error) ())

(define-condition atom-error (request-error)
  ((atom-id :reader atom-error-atom-id :initarg :atom-id))
  (:report
    (lambda (condition stream)
      (report-request-error condition stream)
      (format stream " Atom-ID #x~x" (atom-error-atom-id condition)))))

(define-condition colormap-error (resource-error) ())

(define-condition cursor-error (resource-error) ())

(define-condition drawable-error (resource-error) ())

(define-condition font-error (resource-error) ())

(define-condition gcontext-error (resource-error) ())

(define-condition id-choice-error (resource-error) ())

(define-condition illegal-request-error (request-error) ())

(define-condition length-error (request-error) ())

(define-condition match-error (request-error) ())

(define-condition name-error (request-error) ())

(define-condition pixmap-error (resource-error) ())

(define-condition value-error (request-error)
  ((value :reader value-error-value :initarg :value))
  (:report
    (lambda (condition stream)
      (report-request-error condition stream)
      (format stream " Value ~d." (value-error-value condition)))))

(define-condition window-error (resource-error)())

(define-condition implementation-error (request-error) ())

;;-----------------------------------------------------------------------------
;; Internal error conditions signaled by CLX

(define-condition x-type-error (type-error error)
  ((type-string :reader x-type-error-type-string :initarg :type-string))
  (:report
    (lambda (condition stream)
      (format stream "~S isn't a ~A"
	      (type-error-datum condition)
	      (or (x-type-error-type-string condition)
		  (type-error-expected-type condition))))))

(define-condition closed-display (error)
  ((display :reader closed-display-display :initarg :display))
  (:report
    (lambda (condition stream)
      (format stream "Attempt to use closed display ~s"
	      (closed-display-display condition)))))

(define-condition lookup-error (error)
  ((id :reader lookup-error-id :initarg :id)
   (display :reader lookup-error-display :initarg :display)
   (type :reader lookup-error-type :initarg :type)
   (object :reader lookup-error-object :initarg :object))
  (:report
    (lambda (condition stream)
      (format stream "ID ~d from display ~s should have been a ~s, but was ~s"
	      (lookup-error-id condition)
	      (lookup-error-display condition)
	      (lookup-error-type condition)
	      (lookup-error-object condition)))))  

(define-condition connection-failure (error)
  ((major-version :reader connection-failure-major-version :initarg :major-version)
   (minor-version :reader connection-failure-minor-version :initarg :minor-version)
   (host :reader connection-failure-host :initarg :host)
   (display :reader connection-failure-display :initarg :display)
   (reason :reader connection-failure-reason :initarg :reason))
  (:report
    (lambda (condition stream)
      (format stream "Connection failure to X~d.~d server ~a display ~d: ~a"
	      (connection-failure-major-version condition)
	      (connection-failure-minor-version condition)
	      (connection-failure-host condition)
	      (connection-failure-display condition)
	      (connection-failure-reason condition)))))
  
(define-condition reply-length-error (error)
  ((reply-length :reader reply-length-error-reply-length :initarg :reply-length)
   (expected-length :reader reply-length-error-expected-length :initarg :expected-length)
   (display :reader reply-length-error-display :initarg :display))
  (:report
    (lambda (condition stream)
      (format stream "Reply length was ~d when ~d words were expected for display ~s"
	      (reply-length-error-reply-length condition)
	      (reply-length-error-expected-length condition)
	      (reply-length-error-display condition)))))  

(define-condition reply-timeout (error)
  ((timeout :reader reply-timeout-timeout :initarg :timeout)
   (display :reader reply-timeout-display :initarg :display))
  (:report
    (lambda (condition stream)
      (format stream "Timeout after waiting ~d seconds for a reply for display ~s"
	      (reply-timeout-timeout condition)
	      (reply-timeout-display condition)))))  

(define-condition sequence-error (error)
  ((display :reader sequence-error-display :initarg :display)
   (req-sequence :reader sequence-error-req-sequence :initarg :req-sequence)
   (msg-sequence :reader sequence-error-msg-sequence :initarg :msg-sequence))
  (:report
    (lambda (condition stream)
      (format stream "Reply out of sequence for display ~s.~%  Expected ~d, Got ~d"
	      (sequence-error-display condition)
	      (sequence-error-req-sequence condition)
	      (sequence-error-msg-sequence condition)))))  

(define-condition unexpected-reply (error)
  ((display :reader unexpected-reply-display :initarg :display)
   (msg-sequence :reader unexpected-reply-msg-sequence :initarg :msg-sequence)
   (req-sequence :reader unexpected-reply-req-sequence :initarg :req-sequence)
   (length :reader unexpected-reply-length :initarg :length))
  (:report
    (lambda (condition stream)
      (format stream "Display ~s received a server reply when none was expected.~@
		      Last request sequence ~d Reply Sequence ~d Reply Length ~d bytes."
	      (unexpected-reply-display condition)
	      (unexpected-reply-req-sequence condition)
	      (unexpected-reply-msg-sequence condition)
	      (unexpected-reply-length condition)))))

(define-condition missing-parameter (error)
  ((parameter :reader missing-parameter-parameter :initarg :parameter))
  (:report
    (lambda (condition stream)
      (let ((parm (missing-parameter-parameter condition)))
	(if (consp parm)
	    (format stream "One or more of the required parameters ~a is missing."
		    parm)
	  (format stream "Required parameter ~a is missing or null." parm))))))

;; This can be signalled anywhere a pseudo font access fails.
(define-condition invalid-font (error)
  ((font :reader invalid-font-font :initarg :font))
  (:report
    (lambda (condition stream)
      (format stream "Can't access font ~s" (invalid-font-font condition)))))

(define-condition device-busy (error)
  ((display :reader device-busy-display :initarg :display))
  (:report
    (lambda (condition stream)
      (format stream "Device busy for display ~s"
	      (device-busy-display condition)))))

(define-condition unimplemented-event (error)
  ((display :reader unimplemented-event-display :initarg :display)
   (event-code :reader unimplemented-event-event-code :initarg :event-code))
  (:report
    (lambda (condition stream)
      (format stream "Event code ~d not implemented for display ~s"
	      (unimplemented-event-event-code condition)
	      (unimplemented-event-display condition)))))

(define-condition undefined-event (error)
  ((display :reader undefined-event-display :initarg :display)
   (event-name :reader undefined-event-event-name :initarg :event-name))
  (:report
    (lambda (condition stream)
      (format stream "Event code ~d undefined for display ~s"
	      (undefined-event-event-name condition)
	      (undefined-event-display condition)))))

(define-condition absent-extension (error)
  ((name :reader absent-extension-name :initarg :name)
   (display :reader absent-extension-display :initarg :display))
  (:report
    (lambda (condition stream)
      (format stream "Extension ~a isn't defined for display ~s"
	      (absent-extension-name condition)
	      (absent-extension-display condition)))))

(define-condition inconsistent-parameters (error)
  ((parameters :reader inconsistent-parameters-parameters :initarg :parameters))
  (:report
   (lambda (condition stream)
     (format stream "inconsistent-parameters:~{ ~s~}"
	     (inconsistent-parameters-parameters condition)))))

(defun get-error-key (display error-code)
  (declare (type display display)
	   (type array-index error-code))
  ;; Return the error-key associated with error-code
  (if (< error-code (length *xerror-vector*))
      (svref *xerror-vector* error-code)
    ;; Search the extensions for the error
    (dolist (entry (display-extension-alist display) 'unknown-error)
      (let* ((event-name (first entry))
	     (first-error (fourth entry))
	     (errors (third (assoc event-name *extensions*))))
	(declare (type keyword event-name)
		 (type array-index first-error)
		 (type list errors))
	(when (and errors
		   (index<= first-error error-code
			    (index+ first-error (index- (length errors) 1))))
	  (return (nth (index- error-code first-error) errors)))))))

(defmacro define-error (error-key function)
  ;; Associate a function with ERROR-KEY which will be called with
  ;; parameters DISPLAY and REPLY-BUFFER and
  ;; returns a plist of keyword/value pairs which will be passed on
  ;; to the error handler.  A compiler warning is printed when
  ;; ERROR-KEY is not defined in a preceding DEFINE-EXTENSION.
  ;; Note: REPLY-BUFFER may used with the READING-EVENT and READ-type
  ;;       macros for getting error fields. See DECODE-CORE-ERROR for
  ;;       an example.
  (declare (type symbol error-key)
	   (type function function))
  ;; First ensure the name is for a declared extension
  (unless (or (find error-key *xerror-vector*)
	      (dolist (extension *extensions*)
		(when (member error-key (third extension))
		  (return t))))
    (x-type-error error-key 'error-key))
  `(setf (get ',error-key 'error-decode-function) (function ,function)))

;; All core errors use this, so we make it available to extensions.
(defun decode-core-error (display event &optional arg)
  ;; All core errors have the following keyword/argument pairs:
  ;;    :major integer
  ;;    :minor integer
  ;;    :sequence integer
  ;; In addition, many have an additional argument that comes from the
  ;; same place in the event, but is named differently.  When the ARG
  ;; argument is specified, the keyword ARG with card32 value starting
  ;; at byte 4 of the event is returned with the other keyword/argument
  ;; pairs.
  (declare (type display display)
	   (type reply-buffer event)
	   (type (or null keyword) arg))
  (declare (values keyword/arg-plist))
  display
  (reading-event (event)
    (let* ((sequence (read-card16 2))
	   (minor-code (read-card16 8))
	   (major-code (read-card8 10))
	   (result (list :major major-code
			 :minor minor-code
			 :sequence sequence)))
      (when arg
	(setq result (list* arg (read-card32 4) result)))
      result)))

(defun decode-resource-error (display event)
  (decode-core-error display event :resource-id))

(define-error unknown-error
  (lambda (display event)
    (list* :error-code (aref (reply-ibuf8 event) 1)
	   (decode-core-error display event))))

(define-error request-error decode-core-error) ; 1  bad request code

(define-error value-error		; 2  integer parameter out of range
    (lambda (display event)
      (decode-core-error display event :value)))

(define-error window-error decode-resource-error) ; 3  parameter not a Window

(define-error pixmap-error decode-resource-error) ; 4  parameter not a Pixmap

(define-error atom-error		; 5  parameter not an Atom
    (lambda (display event)
      (decode-core-error display event :atom-id)))

(define-error cursor-error decode-resource-error) ; 6  parameter not a Cursor

(define-error font-error decode-resource-error)	; 7  parameter not a Font

(define-error match-error decode-core-error) ; 8  parameter mismatch

(define-error drawable-error decode-resource-error) ; 9  parameter not a Pixmap or Window

(define-error access-error decode-core-error) ; 10 attempt to access private resource"

(define-error alloc-error decode-core-error) ; 11 insufficient resources

(define-error colormap-error decode-resource-error) ; 12 no such colormap

(define-error gcontext-error decode-resource-error) ; 13 parameter not a GContext

(define-error id-choice-error decode-resource-error) ; 14 invalid resource ID for this connection

(define-error name-error decode-core-error) ; 15 font or color name does not exist

(define-error length-error decode-core-error) ; 16 request length incorrect;
					;    internal Xlib error

(define-error implementation-error decode-core-error) ; 17 server is defective
