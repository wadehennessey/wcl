;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defmacro accumulate-lr (op n args)
  `(loop for i from 1 below ,n
    as result = (,op (svref ,args 0) (svref ,args 1))
    then (,op result (svref ,args i))
    finally (return result)))

(defmacro n-compare (op n args)
  `(loop for i from 1 below ,n
    as result = (,op (svref ,args 0) (svref ,args 1))
    then (,op (svref ,args (- i 1)) (svref ,args i))
    when (null result)
    return result
    finally (return result)))

(defmacro def-nary (name 2-case)
  `(defun ,name (arg-1 &restv args)
    (let ((n (length args)))
      (case n
	(0 arg-1)
	(1 (,2-case arg-1 (svref args 0))) ; just for speed
	(t (,2-case arg-1 (accumulate-lr ,2-case n args)))))))

;;; HEY! Fix so that at least 1 arg is required
(defmacro def-nary-pred (name 2-case)
  `(defun ,name (&restv args)
    (let ((n (length args)))
      (case  n
	((0 1) t)
	(2 (,2-case (svref args 0) (svref args 1)))
	(t (n-compare ,2-case n args))))))

(defconstant boole-clr 0)
(defconstant boole-set 1)
(defconstant boole-1 2)
(defconstant boole-2 3)
(defconstant boole-c1 4)
(defconstant boole-c2 5)
(defconstant boole-and 6)
(defconstant boole-ior 7)
(defconstant boole-xor 8)
(defconstant boole-eqv 9)
(defconstant boole-nand 10)
(defconstant boole-nor 11)
(defconstant boole-andc1 12)
(defconstant boole-andc2 13)
(defconstant boole-orc1 14)
(defconstant boole-orc2 15)

(defstruct (byte (:print-function write-byte-specifier))
  size
  position)

(defstruct (package
	     (:predicate packagep)
	     (:constructor fresh-package)
	     (:print-function write-package))
  name
  symbols
  uses-packages
  used-by-packages
  shadowing-symbols
  nicknames-list
  static-abbreviation)

(defvar *all-packages* nil)

(defvar *lisp-package* (make-package "LISP"
				     :nicknames '("LSP")
				     :use nil
				     :size 12000))

(defvar *keyword-package* (make-package "KEYWORD"
					:nicknames '("KEY")
					:use nil
					:size 1000))

(defvar *user-package* (make-package "USER"
				     :nicknames '("USR")
				     :use '("LISP")
				     :size 1000))

;;; Declared earlier
(setf *package* *user-package*)

(defvar *packages-initialized?* nil)

(defvar *merge-sort-temp-vector* (make-array 50))
(defvar *merge-lists-header* (list :header))


(defvar *standard-input*)
(defvar *standard-output*)
(defvar *error-output*)
(defvar *query-io*)
(defvar *debug-io*)
(defvar *terminal-io*)
(defvar *trace-output*)

(defvar *free-string-buffers* nil)
(defvar *free-string-streams* nil)
(defvar *input-stream-line-numbers?* nil)

(defstruct (stream (:predicate streamp))
  element-type
  element-size
  input-source
  output-sink
  write-char-method
  read-char-method
  write-byte-method
  read-byte-method
  write-float-method
  write-simple-string-method
  force-output-method
  peek-char-method
  unread-char-method
  listen-method)

(defstruct (fptr-stream (:include stream)))

(defstruct (user-stream (:include fptr-stream)))

(defstruct (file-stream (:include fptr-stream))
  pathname)

(defstruct (line-number-stream (:include file-stream))
  (line 1 :type fixnum))

(defstruct (string-stream (:include stream))
  (next 0 :type fixnum)
  (end 0 :type fixnum)
  internal-buffer?)

(defstruct (interactive-stream (:include fptr-stream)))

(defstruct (user-defined-stream (:include fptr-stream)))

;;; Only works for output to interactive streams.
(defstruct (terminal-stream (:include interactive-stream))
  (column 0 :type fixnum))

;;; HEY! Finish other stream types				       
(defstruct (broadcast-stream (:include stream))
  streams-list)

(defstruct (synonym-stream (:include stream))
  symbol)

(defstruct (concatenated-stream (:include stream)))

(defstruct (two-way-stream (:include stream)))

(defstruct (echo-string (:include stream)))

(defconstant default-string-buffer-length 16384)

(defmacro with-valid-stream ((name thing) &body body)
  (let ((v (gensym "TMP")))
    `(let* ((,v ,thing)
	    (,name (cond ((streamp ,v) ,v)
			 ((eq ,v t) *standard-output*)
			 ((null ,v) *standard-input*)
			 (t (error "~A is not a valid stream argument" ,v)))))
      ,@body)))

(defvar *print-escape* t)
(defvar *print-radix* nil)
(defvar *print-base* 10)
(defvar *print-circle* nil)
(defvar *print-pretty* nil)
(defvar *print-level* nil)
(defvar *print-length* nil)
(defvar *print-case* nil)
(defvar *print-gensym* nil)
(defvar *print-array* nil)
(defvar *print-readably* nil)
(defvar *print-right-margin* nil)
(defvar *print-miser-width* nil)
(defvar *print-lines* nil)
(defvar *print-pprint-dispatch* nil)
(defvar *print-structure* nil)


(defvar *readtable*)
(defvar *read-base* 10)
(defvar *read-suppress* nil)
(defvar *open-paren-count* 0)
(defvar *close-paren-marker* "CLOSE-PAREN")
(defvar *dot-marker* "DOT")
(defvar *read-eval* t)
(defvar *read-default-float-format* 'single-float)

(defconstant whitespace 0)
(defconstant constituient 1)
(defconstant multiple-escape 2)
(defconstant single-escape 3)
(defconstant terminating-macro 4)
(defconstant non-terminating-macro 5)

(defconstant alphadigit 0)
(defconstant illegal 1)

(defstruct (readtable (:predicate readtablep)
		      (:copier %copy-readtable))
  (syntax (make-array 256 :initial-element constituient))
  (attributes (make-array 256 :initial-element alphadigit))
  (handlers (make-array 256 :initial-element nil)))

(defstruct macro-char
  function
  non-terminating?)

(defstruct (dispatch-macro-char (:include macro-char))
  sub-functions)

(defmacro get-char-syntax (rt char)
  `(svref (readtable-syntax ,rt) (char-code ,char)))

(defmacro get-char-attributes (rt char)
  `(svref (readtable-attributes ,rt) (char-code ,char)))

(defmacro get-char-macro-info (rt char)
  `(svref (readtable-handlers ,rt) (char-code ,char)))

;;; HEY! Make this a bit-vector like #*101.... when emitter works?
;;; Put this in readtable?
(defvar *character-attributes*
  #(t					; 0 = #\Null
    t					; 1 = #\
    t					; 2 = #\
    t					; 3 = #\
    t					; 4 = #\
    t					; 5 = #\
    t					; 6 = #\
    t					; 7 = #\
    t					; 8 = #\Backspace
    t					; 9 = #\Tab
    t					; 10 = #\Newline
    t					; 11 = #\
    t					; 12 = #\Page
    t					; 13 = #\Return
    t					; 14 = #\
    t					; 15 = #\
    t					; 16 = #\
    t					; 17 = #\
    t					; 18 = #\
    t					; 19 = #\
    t					; 20 = #\
    t					; 21 = #\
    t					; 22 = #\
    t					; 23 = #\
    t					; 24 = #\
    t					; 25 = #\
    t					; 26 = #\
    t					; 27 = #\
    t					; 28 = #\
    t					; 29 = #\
    t					; 30 = #\
    t					; 31 = #\
    t					; 32 = #\Space
    nil					; 33 = #\!
    t					; 34 = #\"
    t					; 35 = #\#
    nil					; 36 = #\$
    nil					; 37 = #\%
    nil					; 38 = #\&
    t					; 39 = #\'
    t					; 40 = #\(
    t					; 41 = #\)
    nil					; 42 = #\*
    nil					; 43 = #\+
    t					; 44 = #\,
    nil					; 45 = #\-
    nil					; 46 = #\.
    nil					; 47 = #\/
    nil					; 48 = #\0
    nil					; 49 = #\1
    nil					; 50 = #\2
    nil					; 51 = #\3
    nil					; 52 = #\4
    nil					; 53 = #\5
    nil					; 54 = #\6
    nil					; 55 = #\7
    nil					; 56 = #\8
    nil					; 57 = #\9
    t					; 58 = #\:
    t					; 59 = #\;
    nil					; 60 = #\<
    nil					; 61 = #\=
    nil					; 62 = #\>
    nil					; 63 = #\?
    nil					; 64 = #\@
    nil					; 65 = #\A
    nil					; 66 = #\B
    nil					; 67 = #\C
    nil					; 68 = #\D
    nil					; 69 = #\E
    nil					; 70 = #\F
    nil					; 71 = #\G
    nil					; 72 = #\H
    nil					; 73 = #\I
    nil					; 74 = #\J
    nil					; 75 = #\K
    nil					; 76 = #\L
    nil					; 77 = #\M
    nil					; 78 = #\N
    nil					; 79 = #\O
    nil					; 80 = #\P
    nil					; 81 = #\Q
    nil					; 82 = #\R
    nil					; 83 = #\S
    nil					; 84 = #\T
    nil					; 85 = #\U
    nil					; 86 = #\V
    nil					; 87 = #\W
    nil					; 88 = #\X
    nil					; 89 = #\Y
    nil					; 90 = #\Z
    t					; 91 = #\[
    t					; 92 = #\\
    t					; 93 = #\]
    nil					; 94 = #\^
    nil					; 95 = #\_
    t					; 96 = #\`
    t					; 97 = #\a
    t					; 98 = #\b
    t					; 99 = #\c
    t					; 100 = #\d
    t					; 101 = #\e
    t					; 102 = #\f
    t					; 103 = #\g
    t					; 104 = #\h
    t					; 105 = #\i
    t					; 106 = #\j
    t					; 107 = #\k
    t					; 108 = #\l
    t					; 109 = #\m
    t					; 110 = #\n
    t					; 111 = #\o
    t					; 112 = #\p
    t					; 113 = #\q
    t					; 114 = #\r
    t					; 115 = #\s
    t					; 116 = #\t
    t					; 117 = #\u
    t					; 118 = #\v
    t					; 119 = #\w
    t					; 120 = #\x
    t					; 121 = #\y
    t					; 122 = #\z
    t					; 123 = #\{
    t					; 124 = #\|
    t					; 125 = #\}
    nil					; 126 = #\~
    t					; 127 = #\Rubout
    ))

;;; INIT!
(setf *readtable* (make-default-readtable))

;;; Boot troubles. Fixup some hashtable stuff that couldn't
;;; work before because they depend on hashtables!
(deftype hash-table () '(satisfies hash-table-p))
(defsetf gethash set-gethash)

;;; Do a copy-list in case someone wants to destructively modify
;;; the constant list (which will end up in the data segment).
(defparameter *features*
  (copy-list '#.(let* ((processor (installation-parameter "PROCESSOR"))
		       (os (installation-parameter "OPERATING_SYSTEM"))
		       (machine-type (processor+os->machine-type processor
								 os)))
		  (list :wcl
			(intern (format nil "WCL~A"
					(library-version (lookup-library :cl)))
				*keyword-package*)
			:unix
			:loop
			(intern (string-upcase machine-type) *keyword-package*)
			(intern (string-upcase processor) *keyword-package*)
			(intern (string-upcase os) *keyword-package*)
			(machine-byte-order machine-type)))))

(defvar *modules* nil)

(defvar *traced-functions* (make-hash-table))

(defvar *trace-level* 0)

(defmacro trace (&rest names)
  `(trace-functions ',names))

(defmacro untrace (&rest names)
  `(untrace-functions ',names))

(defmacro time (form)
  `(print-runtime-statistics (function (lambda () ,form))))

(defconstant unix-to-universal-time-difference 3208988800)
(defconstant internal-time-units-per-second 100)
(defconstant seconds-in-week (* 60 60 24 7))
(defconstant weeks-offset 2145)
(defconstant seconds-offset 432000)
(defconstant minutes-per-day (* 24 60))
(defconstant quarter-days-per-year (1+ (* 365 4)))
(defconstant quarter-days-per-century 146097)
(defconstant november-17-1858 678882)
(defconstant weekday-november-17-1858 2)
(defconstant april-1 (+ (truncate (+ (* (- 4 3) 153) 2) 5) 1))
(defconstant october-31 (+ (truncate (+ (* (- 10 3) 153) 2) 5) 31))

(defmacro with-static-space (&rest forms)
  `(unwind-protect (progn (switch_to_static_space)
			  ,@forms)
    (switch_to_dynamic_space)))

(defvar *cl-version* #.(library-version (lookup-library :cl)))

(defvar *cl-build-date* #.(with-output-to-string (x) (print-time :stream x)))

(defvar *unbound* "UNBOUND")

(defvar *source-pathname* nil)

(defconstant special-forms
  #(block catch eval-when flet function go if labels macrolet
    multiple-value-call mv-bind named-function named-macro-function
    progn quote return-from setq spec-bind tagbody the throw
    unwind-protect values))

(defvar / nil)
(defvar // nil)
(defvar /// nil)
(defvar * nil)
(defvar ** nil)
(defvar *** nil)
(defvar + nil) 
(defvar ++ nil)
(defvar +++ nil)
(defvar - nil)

(defvar *repl-level* 0)

 (defvar *load-verbose* t)

(defvar *constant-redefinition-verbose* t)

(defstruct (physical-pathname
	     (:predicate pathnamep)
	     (:print-function write-pathname))
  host
  device
  directory
  name
  type
  version
  namestring)

(defvar *default-pathname-defaults* nil)

;;; FORMAT decls

(defconstant float-log10-of-2 0.30103)

(defvar *format-control-string* ""
  "The current FORMAT control string")

(defvar *format-index* 0
  "The current index into *format-control-string*")

(defvar *format-length* 0
  "The length of the current FORMAT control string")

(defvar *format-arguments* ()
  "Arguments to the current call of FORMAT")

(defvar *format-original-arguments* ()
 "Saved arglist from top-level FORMAT call for ~* and ~@*")

(defvar *format-stream-stack* ()
  "A stack of string streams for collecting FORMAT output")

(defvar *format-dispatch-table* ()
  "Dispatch table for FORMAT commands")

(defvar cardinal-ones
  '#(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
  "Table of cardinal ones-place digits in English")

(defvar cardinal-tens
  '#(nil nil "twenty" "thirty" "forty"
     "fifty" "sixty" "seventy" "eighty" "ninety")
  "Table of cardinal tens-place digits in English")

(defvar cardinal-teens
  '#("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
     "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")
  "Table of cardinal 'teens' digits in English")

(defvar cardinal-periods
  '#("" " thousand" " million" " billion" " trillion" " quadrillion"
     " quintillion" " sextillion" " septillion" " octillion" " nonillion"
     " decillion")
  "Table of cardinal 'illions' in English")

(defvar ordinal-ones
  '#(nil "first" "second" "third" "fourth"
     "fifth" "sixth" "seventh" "eighth" "ninth")
  "Table of ordinal ones-place digits in English")

(defvar ordinal-tens
  '#(nil "tenth" "twentieth" "thirtieth" "fortieth"
     "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth")
  "Table of ordinal tens-place digits in English")


(defconstant random-state-initial-data 
  '(#x3 #xa319039 #x2d9c024 #x9663182 #x5a1f342
    #x749e56b #xbb1dbb0 #xa5c5918 #x96554fd
    #x82e680f #xe3d799f #xb1ee0b7 #x2436b86
    #xd672e2a #x188ca88 #xe69735d #x94f35f7
    #xd158fd6 #x6a6f051 #x66e6b96 #xa94efdc
    #xd3b81e0 #xd0a6fb5 #xf03bc02 #x4f340fb
    #x3413f93 #xc22c298 #xfa42ab8 #x888d77b
    #xfad9d0e #x899220b #x2fb47b9))

(defstruct (random-state (:constructor fresh-random-state)
			 (:print-function write-random-state)
			 (:copier nil))
  seed
  data)

;;; HEY! This causes a memory overwrite that hasn't been debugged and fixed
;;; yet, so we comment it out for now and leave *random-state* unusable
;;; (defvar *random-state* (make-random-state t))
(defvar *random-state* nil)

(defstruct lambda-list
  requireds
  optionals
  rest
  restv
  keys
  auxes
  specials
  allow-other-keys?)

(defvar *eval-macro-env* (make-macro-env))

(defvar *inspect-print-length* 20)

(defvar *open-shared-object-files* (make-hash-table :test #'equal))

(defun complete-delayed-defstructs ()
  ;; Also complete package init, since defstruct completion expects
  ;; symbol-package cell to be correct! Bletch!
  (unless *packages-initialized?*
    (initialize-package-structures))
  (when *delay-structure-defs?*
    (loop for s in (nreverse *delayed-structure-defs*)
	  do (complete-structure-def s))
    (setf *delayed-structure-defs* nil)
    (setf *delay-structure-defs?* nil)))

(defun finish-boot ()
  (complete-delayed-defstructs))

(defvar *booting?* t)

(when *booting?*
  (initialize-format)
  (initialize-streams)
  (setf *default-pathname-defaults* (current-default-pathname))
  (setf *booting?* nil))









