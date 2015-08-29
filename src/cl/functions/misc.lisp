;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; Bletch! Find a better way to do this with foreign support
(add-winfo ":sf APPLY \"p_lsp_APPLY\"") 
(add-winfo ":sf EVAL-APPLY \"p_lsp_EVAL_2DAPPLY\"") 
(add-winfo ":sf FUNCALL \"p_lsp_FUNCALL\"")

(defun collect-cases (cases)
  (loop for (case . ignore) in cases
	appending (if (listp case)
		      case
		      (list case))))

(defun processor+os->machine-type (processor os)
  (let ((type (cond ((string-equal os "Linux") :linux-pc)
		    ((string-equal os "sunos")
		     (cond ((string-equal processor "sparc") :sun-4))))))
    (if (null type)
	(warn "Machine type not known for ~A running ~A" processor os)
	type)))

(defun unix->lisp-path-list (string)
  (let ((len (length string)))
    (labels ((doit (start)
	       (if (>= start len)
		   nil
		   (let ((end (or (position #\: string :start start) len)))
		     (cons (subseq string start end)
			   (doit (1+ end)))))))
      (doit 0))))

(defun setf-function-symbol (function-specifier)
  (if (consp function-specifier)
      (let ((accessor (second function-specifier)))
	(or (gethash accessor *setf-methods*)
	    (intern (format nil "SET-~A" accessor)
		    (symbol-package accessor))))
      function-specifier))

(defun set-fdefinition (new-value function-specifier)
  (if (consp function-specifier)
      (progn
        (setf (symbol-function (setf-function-symbol function-specifier))
              new-value)
	;; HEY! fix this to not use EVAL!!! Cheap hack for now...
        (eval `(defsetf ,(cadr function-specifier) 
                        (&rest all-args)
                        (new-value)
                 `(,',(setf-function-symbol function-specifier)
                   ,new-value
                   ,@all-args))))
      (setf (symbol-function function-specifier) new-value)))

(defun cd (&optional (pathname (getenv "HOME")))
  (unless (null pathname)
    (unless (= (chdir (namestring pathname)) 0)
      (error "Cannot change to directory ~A" pathname))
    (let ((default (pwd)))
      (setf *default-pathname-defaults* (current-default-pathname))
      default)))

(defun coerce-to-function (f)
  (etypecase  f
    (procedure f)
    (symbol (symbol-function f))))

(defun compiled-function-name (f)
  (let ((proc (etypecase f
		(symbol (symbol-function f))
		(procedure f))))
    (pc->procedure-name (procedure-code proc))))

(defun connect-to-server (host display)
  (connect_to_server host display))

(defun constantp (x)
  (cond ((symbolp x) (constant-var? x))
	((atom x) t)
	(t (and (listp x)
		 (eq (car x) 'quote)
		 (= (length x) 2)))))

(defun copy-structure (old)
  (let* ((len (%object-length old))
	 (new (new-structure len)))
    (%32bit-def new 0 (structure-type old))
    (loop for i from 1 below len
	  do (%32bit-def new i (%32bit-ref old i)))
    new))

(defun-inline foreign-pointer-type (x)
  (%32bit-ref x 1))

(defun-inline foreign-pointer-pointer (x)
  (%32bit-ref x 0))

(defun current-default-pathname ()
  (merge-pathnames (pwd) ".lisp"))

(defun disassemble (function)
  (warn "If you want to disassemble ~A, compile it and read the C code"
	function))

(defun function-lambda-expression (name)
  (and (fboundp name)
       (get name :function-definition)))

(defun uncompile (name)
  (let ((def (function-lambda-expression name)))
    (if (null def)
	(error "Cannot find source code for ~A" name)
	(eval def))))

(defun lisp-implementation-type () "WCL")

(defun lisp-implementation-version () *cl-version*)

(defun machine-instance ()
  (let* ((buffer (make-string unix-maxpathlen))
	 (status (gethostname buffer unix-maxpathlen)))
    (let ((end (position #\Null buffer)))
      (subseq buffer 0 end))))

(defun os-version ()
  (let* ((buffer (make-string unix-maxpathlen))
	 (status (getosversion buffer unix-maxpathlen)))
    (let ((end (position #\Null buffer)))
      (subseq buffer 0 end))))

(defun short-site-name ()
  #.(installation-parameter "SHORT_SITE_NAME"))

(defun long-site-name ()
  #.(installation-parameter "LONG_SITE_NAME"))

(defvar *wcl-version*
  #.(installation-parameter "WCL_VERSION"))

(defun feature? (x)
  (member (if (symbolp x)
	      (intern (symbol-name x) *keyword-package*)
	      x)
	  *features*))

(defun gc-call-count ()
  (gc_call_count))

(defun gc ()
  (full_gc))

(defun-inline identity (x) x)

;;; HEY! move this to structure stuff.
(defun structure-slot-names (s)
  (mapcar #'struct-slot-name (all-slots (lookup-structure-info (type-of s)))))

(defun describe (x)
  (format t "~A is a ~A~%" x (type-of x)))

(defun inspect (x)
  (catch 'exit-inspector
    (inspect-1 x))
  x)

(defun inspect-1 (x)
  (let ((* x))
    (typecase x
      (symbol (inspect-symbol x))
      (number (inspect-number x))
      (character (inspect-character x))
      (list (inspect-list x))
      (sequence (inspect-sequence x))
      (structure (inspect-structure x))
      (foreign-pointer (inspect-foreign-pointer x))
      (t (inspect-anything x)))))

(defun inspect-command-loop (x accessor)
  (format t "~%Command (:H for help): ")
  (let ((cmd (read)))
    (labels ((help ()
	       (format
		t
		"~%:Q quits~%<number> inspects a slot~%~
                 :U goes up one level~%* is bound to the current object~%~
                 other expression are evaluated~%"))
	     (not-a-cmd ()
	       (print (eval cmd))
	       ;; (format t "~A is not an inspector command~%" cmd)
	       ;; (help)
	       (inspect-command-loop x accessor)))
      (typecase cmd
	(symbol (case cmd
		  (:H (help) (inspect-command-loop x accessor))
		  (:Q (throw 'exit-inspector nil))
		  (:U nil)
		  (t (not-a-cmd))))
	(integer (if (null accessor)
		     (progn
		       (format t
			       "No sub objects are available for inspection~%")
		       (inspect-command-loop x accessor))
		     (multiple-value-bind (e legal?)
			 (funcall accessor cmd)
		       (if legal?
			   (progn (inspect-1 e)
				  (inspect-1 x))
			   (progn
			     (format t "~D is not a legal slot number" cmd)
			     (inspect-command-loop x accessor))))))
	(t (not-a-cmd))))))
    
(defun inspect-banner (x type &optional (blurb "a"))
  (format t "Inspecting ~A ~A at address #x~X~%~%"
	  blurb type (object->pointer x)))

(defun inspect-symbol (x)
  (inspect-banner x 'symbol)
  (format t "[0: NAME] ~S~%" (symbol-name x))
  (if (boundp x)
      (format t "[1: VALUE] ~S~%" (symbol-value x))
      (format t "[1: VALUE] #<unbound value>~%"))
  (if (fboundp x)
      (format t "[2: FUNCTION] ~S~%" (symbol-function x))
      (format t "[2: FUNCTION] #<unbound function handler>~%"))
  (format t "[3: PLIST] ~S~%" (symbol-plist x))
  (format t "[4: PACKAGE] ~S~%" (symbol-package x))
  (inspect-command-loop
   x
   #'(lambda (i)
       (if (< i 5)
	   (case i
	     (0 (values (symbol-name x) t))
	     (1 (if (boundp x)
		    (values (symbol-value x) t)
		    (progn (format t "~S does not have a value~%" x)
			   (values nil nil))))
	     (2 (if (fboundp (symbol-function x))
		    (values (symbol-function x) t)
		    (progn (format t
				   "~S does not have a function definition~%")
			   (values nil nil))))
	     (3 (values (symbol-plist x) t))
	     (4 (values (symbol-package x) t)))
	   (values nil nil)))))

(defun inspect-number (x)
  (let ((type (etypecase x
		(fixnum 'fixnum)
		(bignum 'bignum)	
		(float 'float))))
    (inspect-banner x type)
    (format t "value: ~D~%" x)
    (inspect-command-loop x nil)))

(defun inspect-character (x)
  (inspect-banner x 'character)
  (format t "character name: ~C~%" x)
  (inspect-command-loop x nil))

(defun inspect-list (x)
  (if (listp (cdr x))
      (if (null (cdr (last x)))
	  (inspect-sequence x)
	  (inspect-dotted-list x))
      (inspect-cons x)))

(defun inspect-cons (x)
  (inspect-banner x 'cons)
  (format t "[0: CAR] ~S~%" (car x))
  (format t "[1: CDR] ~S~%" (cdr x))
  (inspect-command-loop x #'(lambda (i)
			      (case i
				(0 (values (car x) t))
				(1 (values (cdr x) t))
				(t (values nil nil))))))

(defun inspect-dotted-list (x)
  (inspect-banner x 'dotted-list)
  (inspect-command-loop x nil))

(defun inspect-sequence (x)
  (inspect-banner x (if (listp x) 'list (type-of x)))
  (let ((len (length x)))
    (loop for i from 0 below (min len *inspect-print-length*)
	  do (format t "[~D] ~S~%" i (elt x i)))
    (when (> len *inspect-print-length*)
      (format t "~D more elements...~%" (- len *inspect-print-length*)))
    (inspect-command-loop x
			  #'(lambda (i)
			      (if (< i len)
				  (values (elt x i) t)
				  (values nil nil))))))

(defun inspect-structure (x)
  (let* ((structure-name (type-of x))
	 (slot-names (structure-slot-names x)))
    (inspect-banner x structure-name "a structure of type")
    (let ((len (length slot-names)))
      (loop for i from 0 below (min len *inspect-print-length*)
	    for name in slot-names
	    do (format t "[~D: ~A] ~S~%"
		       i name (structure-elt x (+ i 1))))
      (when (> len *inspect-print-length*)
	(format t "~D more slots...~%" (- len *inspect-print-length*)))
      (inspect-command-loop x
			    #'(lambda (i)
				(if (< i len)
				    (values (structure-elt x (+ i 1)) t)
				    (values nil nil)))))))

(defun inspect-foreign-pointer (x)
  (inspect-banner x (foreign-pointer-type x) "a foreign structure of type")
  ;; HEY! print out fields
  (inspect-command-loop x nil))

(defun inspect-anything (x)
  (inspect-banner x (type-of x))
  (inspect-command-loop x nil))

(defun machine-byte-order (machine-type)
  (case machine-type
    (:linux-pc :little-endian)
    (:sun-4 :big-endian)))

;;; Need to setup packages and symbols correctly before loading the .so file
;;; so the loader can correctly intern new symbols.
(defun setup-packages-and-symbols-before-loading (filename)
  (let ((symtab-name (format nil "/tmp/symtab-~D.ld" (getpid)))
	(default-script-name "/tmp/default-ld-script")
	(symcount 0))
    (shell (format nil "ld --verbose > ~A" default-script-name))
    (with-open-file (symtab-output symtab-name :direction :output)
      (with-open-file (input (merge-pathnames ".wcl" filename))
	(read-line input)		; discard "/*"
	(loop for cmd = (read input nil input)
	      until (eq cmd :end)
	      do (let ((rest (read input)))
		   (read-line input)
		   (case cmd
			 (:package (eval rest))
			 (:sym (incf symcount)
			       (format symtab-output
				       "~A = 0x~X;~%"
				       (lisp->c-symbol-name rest)
				       ;;; un-LREF it - what func for this?
				       ;;; Total HACK!
				       (- (object->pointer rest) #x0)))
			 (t nil)))))
      (with-open-file (default default-script-name)
	(let ((copy? nil))
	  (loop for line = (read-line default nil default)
		until (eq line default)
		do (if (and (> (length line) 0)
			    (char= (schar line 0) #\=))
		       (setf copy? (not copy?))
		     (when copy?
		       (write-line line symtab-output)))))))
    (if (= symcount 0)
	nil
        symtab-name)))

(defun create-shared-object-for-loading (so-name symtab-name)
  (let ((o-name (make-pathname :defaults so-name :type "o")))
    (let ((status
	   (shell
	    (format nil
		    "gcc -shared ~A -Wl,-soname,~A -o ~A ~A -lc -lm -lnsl"
		    (if (null symtab-name)
			""
		        (format nil "-Wl,\"-T,~A\"" symtab-name))
		    so-name
		    so-name
		    o-name))))
      (unless (successful-status? status)
	(error "Cannot create ~S for loading" so-name))
      so-name)))
	   

(defun do-runtime-load-commands (so-handle filename)
  (with-open-file (input (merge-pathnames ".wcl" filename))
    (read-line input)			; discard "/*"
    (loop for cmd = (read input nil input)
	  until (eq cmd :end)
	  do (let ((symbol (read input)))
	       (case cmd
		 ((:sym :comment nil))
		 (:proclaim (proclaim (eval symbol)))
		 (:pinfo (read-line input)) ; discard pinfo
		 (:version nil)		; check it!
		 (:init (funcall (symbol-function symbol)))
		 (:end-package-info nil)
		 (:package nil)		; already done
		 (:finfo (read-line input))
		 (:c-type (read input))
		 (:structure	
		  (let ((fluid-predicate-c-name (read input)))
		    (setf (symbol-function (predicate-name symbol))
			  (make-procedure-object
			   ;;; code ptr is raw
			   (c-symbol-value so-handle
					   fluid-predicate-c-name)))))
		 (t (let ((label (read input)))
		      (ecase cmd
			((:sf :sm) 
			 (when (eq cmd :sm)
			   (set-macro-name-flag symbol))  
			 (let ((code-ptr (c-symbol-value so-handle label)))
			   ;;; code-ptr is a raw ptr!
			   (if (= code-ptr 0)
			       (error "~A is undefined" symbol)
			       (setf (symbol-function symbol) 
				     (make-procedure-object code-ptr)))))
			(:sc (define-variable symbol label nil :constant))
			;; HEY! need to distinguish :var and :parameter
			(:sv (define-variable symbol label nil :var))))))))))


(defun load (file &key (verbose *load-verbose*) print if-does-not-exist)
  (let ((type (pathname-type file))
	(*package* *package*))
    (if (or (string= type "o")
	    (and (null type)
		 (probe-file (merge-pathnames file ".o"))))
	(load-lisp-object-file file verbose)
	(load-lisp-file file verbose print))))

(defun load-lisp-file (file verbose print)
  (let ((*source-pathname* (merge-pathnames file *default-pathname-defaults*)))
    (with-open-file (s *source-pathname*)
      (unless (null verbose)
	(format *standard-output* "~&Loading ~S" *source-pathname*))
      (loop for form = (read s nil s)
	    until (eq form s)
	    do (let ((value (eval form)))
		 (unless (null print)
		   (print value))))
      (pathname s))))

(defun load-lisp-object-file (filename verbose)
  (let ((*source-pathname*
	 (make-pathname :defaults
			(merge-pathnames filename
					 *default-pathname-defaults*)
			:type "so")))
    (unless (null verbose)
      (format *standard-output* "~&Loading ~S" *source-pathname*))
    (let ((open-handle (gethash *source-pathname* *open-shared-object-files*)))
      (when open-handle
	;;;(close-object-file open-handle)
	))
    (let ((symtab-name (setup-packages-and-symbols-before-loading filename)))
      (create-shared-object-for-loading *source-pathname* symtab-name))
    (let ((so-handle (open-object-file *source-pathname*)))
      (setf (gethash *source-pathname* *open-shared-object-files*)
	    so-handle)
      (do-runtime-load-commands so-handle filename))
    *source-pathname*))

(defun load-init-file ()
  (let ((init-file (merge-pathnames (concatenate 'string (getenv "HOME") "/")
				    ".wclinit")))
    (when (probe-file init-file)
      (load init-file :verbose nil))))


(defun load-compiled-file (filename)
  ;; flags: RTLD_LAZY | RTLD_GLOBAL (0x1 | 0x100
  (dlopen filename #x101))

(defun open-object-file (filename)
  (let ((handle (load-compiled-file (namestring filename))))
    (if (= handle 0)
	(error "Cannot load ~S" filename)
        handle)))

(defun close-object-file (handle)
  (let ((result (dlclose handle)))
    (format t "closing ~x, result ~x~%" handle result)))
	

(defun machine-type ()
  #.(processor+os->machine-type (installation-parameter "PROCESSOR")
				(installation-parameter "OPERATING_SYSTEM")))

;;; UG! MAKE-PROCEDURE makes a structure used by the compiler
;;; This makes an open procedure (doesn't point to a closure)
(defun make-procedure-object (code-pointer)
  (let ((p (alloc_words 1 type-procedure)))
    (%32bit-def p 0 code-pointer)
    p))

(defun-inline set-procedure-type-flag (p flag)
  (%set-object-length p flag))

(defun-inline oe-ref (oe i)
  (%32bit-ref oe i))

(defun-inline set-oe-ref (oe i value)
  (%32bit-def oe i value)
  value)

(defun-inline new-structure (len)
  (alloc_words len type-structure))

(defun object->pointer (x)
  (%pointer x))

(defun pc->procedure-name (pc)
  (error "pc->procedure-name not implemented"))

(defvar *greeting-names*
  #("Western"
    "Wakeegan"
    "Warlock"
    "Wild"
    "World"
    "Wierd"
    "Wiley"
    "Warsaw"
    "Warped"
    "Waylaid"
    "Wolf"
    "Why"
    "Whiskey"
    "Wanton"
    "Warey"
    "Wrangler's"
    "Walla"
    "Wailing"
    "Woolly"
    "Wrecked"
    "Wretched"
    "What"
    "Walking"
    "Which"
    "Wet"
    "Wee"
    "Waylaid"
    "Wayout"
    "Warm"
    "Wary"
    "Wyoming"
    "Wanna"
    "Wont"
    "Willing"
    "Wobegone"
    "Were"
    "Was"
    "Worm"
    "Who"))

(defun random-w-name ()
  (aref *greeting-names* (random (length *greeting-names*))))

(defun print-structure (s)
  (print s))

;;; INCREDIBLE! Common Lisp doesn't provide a standard function
;;; for printing the time of day out to a stream!
;;; I'm suprised there isn't a format directive to do this...
(defun print-time (&key (stream t) (universal-time (get-universal-time))
			24-hour-time)
  (multiple-value-bind (seconds
			minutes
			hours
			day
			month
			year
			day-of-week
			daylight-savings
			time-zone)
      (decode-universal-time universal-time)
    (declare (ignore daylight-savings time-zone))
    (let ((am? (< hours 12)))
      (format stream "~A:~2,'0D:~2,'0D ~Aon ~A, ~A ~A, ~A"
	      (if 24-hour-time
		  hours
		  (let ((h (if am? hours (- hours 12))))
		    (if (= h 0) 12 h)))
	      minutes
	      seconds
	      (if 24-hour-time
		  ""
		  (if am? "am " "pm "))
	      (svref #("Monday" "Tuesday" "Wednesday" "Thursday"
		       "Friday" "Saturday" "Sunday")
		     day-of-week)
	      (svref #("January" "February" "March" "April" "May"
		       "June" "July" "August" "September" "October"
		       "November" "December")
		     (1- month))
	      day
	      year))))

(defmethod procedure-code ((p procedure))
  (object->pointer (%32bit-ref p 0)))

(defun proclaim (decl-spec)
 (if (fboundp 'proclaim-w)
     ;; Allow static procedure linking
     (funcall (symbol-function 'proclaim-w) decl-spec)
     ;; Only handle SPECIALS if compiler isn't present
     (when (eq (car decl-spec) 'special)
	  (loop for special in (cdr decl-spec)
		     do (proclaim-special-variable special))))
 t)

(defun provide (module-name)
  (pushnew module-name *modules* :test #'equal))

(defun pwd ()
  (let ((buffer (make-string unix-maxpathlen)))
    (getcwd buffer unix-maxpathlen)
    (let ((end (position #\Null buffer)))
      (setf (schar buffer end) #\/)	; include trailing slash
      (pathname (subseq buffer 0 (1+ end))))))

;;; SPICE based
(defun query-readline ()
  ;; The string below contains a tab followed by a space.
  (string-trim "	 " (read-line *query-io*)))

(defun y-or-n-p (&optional format-string &rest arguments)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments)
    (force-output *query-io*))
  (loop
   (let* ((line (query-readline))
	  (ans (if (string= line "")
		   #\?			;Force CASE below to issue instruction.
		   (schar line 0))))
     (case ans
       ((#\y #\Y) (return t))
       ((#\n #\N) (return nil))
       (t (write-line "Type \"y\" for yes or \"n\" for no. " *query-io*)
	  (when format-string
	    (apply #'format *query-io* format-string arguments))
	  (force-output *query-io*))))))

(defun yes-or-no-p (&optional format-string &rest arguments)
  ;; (clear-input *query-io*)
  ;; (beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (loop
   (let ((ans (query-readline)))
     (cond ((string-equal ans "YES") (return t))
	   ((string-equal ans "NO") (return nil))
	   (t (write-line "Type \"yes\" for yes or \"no\" for no. "
			  *query-io*)
	      (when format-string
		(apply #'format *query-io* format-string arguments)))))))


(defun user-homedir-pathname ()
  (getenv "HOME"))

(defun quit ()
  (exit 0))
 
(defun random (n &optional (random-state *random-state*))
  (if (random-state-p random-state)
      (progn (setstate (random-state-data random-state))
	     (let ((r (abs (c_random))))
	       (mod r n)))
      (error "~A should be a random-state" random-state)))

(defun copy-random-state (s)
  (let* ((old-data (random-state-data s))
	 (len (length old-data))
	 (new-data (make-array len
			       :element-type '(unsigned-byte 32))))
    (replace new-data old-data)
    (fresh-random-state
     :seed (random-state-seed s)
     :data new-data)))

(defun make-random-state (&optional random-state)
  (let* ((s (cond ((null random-state)
		   (copy-random-state *random-state*))
		  ((random-state-p random-state)
		   (copy-random-state random-state))
		  ((eq random-state t)
		   (fresh-random-state
		    :seed (rem (get-universal-time) most-positive-fixnum)
		    :data (make-array
			   (length random-state-initial-data)
			   :element-type '(unsigned-byte 32)
			   :initial-contents random-state-initial-data)))
		  (t (error "~A is an illegal argument"))))
	 (data (random-state-data s)))
    (initstate (random-state-seed s) data (* (length data) 4))
    s))

(defun repl-1 (init exit-forms prompt-function &optional venv fenv tenv benv)
  (let ((+ nil)
	(++ nil)
	(+++ nil)
	(/// nil)
	(// nil)
	(/ nil)
	(* init)
	(*** nil)
	(* nil)
	(level *repl-level*))
    (loop
     (terpri)
     (princ (funcall prompt-function))
     (let ((x (read *standard-input* nil *standard-input*)))
       ;; Munch newline if present so that input queries, readline, etc.
       ;; work "as expected" (= like othe lisps!).
       (let ((next (read-char *standard-input*)))
	 (if (char= next #\Newline)
	     ;; BARF! Find a way to do this cleanly...
	     (setf (terminal-stream-column *standard-output*) 0)
	     (unread-char next *standard-input*)))
       (if (or (member x exit-forms) (eq x *standard-input*))
	   (return-from repl-1 (if (eq x *standard-input*) :eof x))
	   (progn
	     (setf +++ ++
		   ++ +
		   + x)
	     (with-simple-restart (abort "Abort to level ~D" level)
	       (multiple-value-call #'(lambda (&rest values)
					(setf /// //
					      // /
					      / values
					      *** **
					      ** *
					      * (car values))
					(fresh-line *standard-output*)
					(loop for x in values 
					      do (progn (prin1 x) (terpri))))
		 (eval-1 x venv fenv tenv benv)))))))))

(defun repl-prompt ()
  "> ")

(defun debug-prompt ()
  (format nil "Eval:~D> " *repl-level*))

(defun repl ()
  (loop (let ((value (catch 'top-level (repl-1 nil nil #'repl-prompt))))
	  (case value
	    (:eof (quit))))))

(defun require (module-name &optional pathname)
  (unless (member module-name *modules* :test #'equal)
      (load pathname))
  module-name)

(defun heap-page-size ()
  (heap_page_size))

(defun total-heap-pages ()
  (total_heap_pages))

(defun free-heap-pages ()
  (free_heap_pages))

(defun percentage (x y)
  (truncate (* (/ x y) 100)))

(defun room (&optional verbose)
  (let* ((page-size (heap-page-size))
	 (total (* (total-heap-pages) page-size))
	 (free (* (free-heap-pages) page-size))
	 (used (- total free))
	 (static-total (* page-size (total_static_pages)))
	 (static-free (free_static_bytes)))
    (format t "Total heap size is ~Dk bytes~%" (floor total 1024))
    (format t "~Dk bytes are free (~D%)~%"
	    (floor free 1024) (percentage free total))
    (format t "~Dk bytes are used (~D%)~%"
	    (floor used 1024) (percentage used total))
    (format t "~%Total static space size is ~Dk bytes~%"
	    (floor static-total 1024))
    (format t "~Dk bytes are free~%"
	    (floor static-free 1024))))
      
(defun set-gc-messages (flag)
  (set_gc_messages (if flag 1 0))
  flag)

;;; Use the SYSTEM function if you want to start an interactive subshell.
;;; However, System uses fork, and often the Lisp process is too big to
;;; copy, so the system command fails. Hence, popen/pclose is the
;;; best way to run an inferior shell command such as a c compile or a link.

(defun shell (&optional (command "csh"))
  (run-program command))

;;; This is a total hack for now. 
(defun run-program (command)
  ;; "rw" doesn't work - should be either "r" or "w" ???
  (let ((file-ptr (popen command "w")))	
    (pclose file-ptr)))

(defun sleep (n)
  (let ((microseconds (round (* 1000000 n))))
    (usleep microseconds)
    nil))

(defun software-type ()
  "Unix")

(defun special-form-p (name)
  (find name special-forms :test #'eq))

(defmethod-inline structure-type ((s structure))
  (%32bit-ref s 0))

(defun tmpdir ()
  (or (getenv "TMPDIR") "/tmp"))

(defun trace-function (name)
  (if (fboundp name)
      (let ((already-traced? (gethash name *traced-functions*)))
	(if already-traced?
	    (warn "The function ~A is already being traced" name)
	    (let ((old (symbol-function name)))
	      (setf (gethash name *traced-functions*) old)
	      (setf (symbol-function name)
		    #'(lambda (&rest args)
			(declare (dynamic-extent args))
			(let* ((*trace-level* (+ *trace-level* 1))
			       (indent (* *trace-level* 2)))
			  (format *trace-output*
				  "~&~VT~D> (~A ~{ ~S~})~%"
				  indent *trace-level* name args)
			  (force-output *trace-output*)
			  (multiple-value-call
			      #'(lambda (&rest values)
				  (declare (dynamic-extent values))
					  (format *trace-output*
					  "~&~VT~D< returned: ~{ ~S~}~%"
					  indent *trace-level* values)
					  (force-output *trace-output*)
				  (values-list values))
			    (apply old args)))))
	      (list name))))
      (warn "The function ~A is undefined" name)))
      
(defun untrace-function (name)
  (let ((old (gethash name *traced-functions*)))
    (if (null old)
	(warn "The function ~A is not being traced" name)
	(progn (remhash name *traced-functions*)
	       (setf (symbol-function name) old)
	       (list name)))))

(defun traced-function-names ()
  (let ((l nil))
    (maphash #'(lambda (name value) (push name l)) *traced-functions*)
    l))

(defun trace-functions (names)
  (if (null names)
      (traced-function-names)
      (loop for name in names appending (trace-function name))))

(defun untrace-functions (names)
  (if (null names)
      (let ((new-names (traced-function-names)))
	(if (null new-names)
	    nil
	    (untrace-functions new-names)))
      (loop for name in names appending (untrace-function name))))

(defun values-list (l)
  (apply #'values l))

;;; This is like APPLY on a continuation, hence the same ugliness,
;;; but we could write this more efficiently in C as a loop.
(defun values (&restv v)		
  (case (length v)
    (0 (values))
    (1 (values (svref v 0)))
    (2 (values (svref v 0) (svref v 1)))
    (3 (values (svref v 0) (svref v 1) (svref v 2)))
    (4 (values (svref v 0) (svref v 1) (svref v 2) (svref v 3)))
    (5 (values (svref v 0) (svref v 1) (svref v 2) (svref v 3) (svref v 4)))
    (6 (values (svref v 0) (svref v 1) (svref v 2) (svref v 3) (svref v 4)
	       (svref v 5)))       
    (7 (values (svref v 0) (svref v 1) (svref v 2) (svref v 3) (svref v 4)
	       (svref v 5) (svref v 6)))
    (8 (values (svref v 0) (svref v 1) (svref v 2) (svref v 3) (svref v 4)
	       (svref v 5) (svref v 6) (svref v 7)))
    (9 (values (svref v 0) (svref v 1) (svref v 2) (svref v 3) (svref v 4)
	       (svref v 5) (svref v 6) (svref v 7) (svref v 8)))
    (t (error "HEY! Write runtime for VALUES in C"))))

(defun ref-structure-as-vector (s i)
  (structure-elt s (1+ i)))

(defun start-application (main-function)
  (catch 'top-level (funcall main-function)))

(defun set-documentation (name type string)
  nil)

(defun documentation (name type)
  nil)

(defun c-symbol-value (so-handle name)
  (dlsym so-handle name))

(defun command-line-argument (n)
  (command_line_argument n))
 
(defun executable-root-directory ()
  (let* ((s (command-line-argument 0))
	 (pos-1 (position #\/ s :from-end t)))
    (if (null pos-1)
	""
        (let* ((pos-2 (position #\/ s :from-end t :end pos-1))
	       (end (or pos-2 pos-1)))
	  (concatenate 'string (subseq s 0 end))))))

(defun rename-file (&rest ignore)
  (error "Sorry, `rename-file' is not yet implemented"))

(defun dribble (&rest ignore)
  (error "Sorry, `dribble' is not yet implemented"))


