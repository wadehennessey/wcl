;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defvar *current-line* nil)

(defstruct code
  tail?
  mv-holder
  (out-type t) 
  (line *current-line*)
  result-name)

(defstruct (c-code (:include code))
  string)

(defstruct (seq (:include code)
		(:copier nil))		; don't trash CL COPY-SEQ
  body)					; forms, control points and transfers

(defstruct (values-seq (:include seq))
  values)

(defstruct (progn (:include seq)))

(defstruct (scope-seq (:include seq))	; block and catch
  control-point)

(defstruct (tag-seq (:include seq)) ; tagbody
  control-points)

;;; Multiple-inheritance would be nice here (static/dynamic, block/catch/tag)
(defstruct (control-point (:include code))
  name
  parent				; owning scope
  refs					; list of nodes which ref us
  id)

(defstruct (scope-control-point (:include control-point))
  receive-var)

(defstruct (static-scope-control-point (:include scope-control-point))
  c-name
  convert?)				; T if should convert to dynamic

(defstruct (dynamic-scope-control-point (:include scope-control-point))
  tag-name)

(defstruct (dynamic-block-control-point
	     (:include dynamic-scope-control-point)))

(defstruct (catch-control-point (:include dynamic-scope-control-point)))

(defstruct (tag-control-point (:include control-point))
  c-name)

(defstruct (static-tag-control-point (:include tag-control-point))
  convert?)

(defstruct (dynamic-tag-control-point (:include tag-control-point))
  tag-name)

(defstruct (control-transfer (:include code)) 
  destination-point
  unwind-count)

(defstruct (scope-control-transfer (:include control-transfer))
  send-value)

(defstruct (tag-control-transfer (:include control-transfer)))

(defstruct (unwind-protect (:include code))
  protected-form
  cleanup-form)

(defstruct (spec-bind-seq (:include values-seq))
  specials)

(defstruct (proc (:include seq))
  var-info
  name
  c-name
  max-tmp-var-count
  vars-to-declare
  funarg-refs				; list of funarg refs
  oe-var				; null if we don't need it
  start-label				; only needed for tail rec removal
  volatile)			

(defstruct (inner-proc (:include proc))
  oe-refs
  pass-on-oe?
  parent-chain)

(defstruct (top-level-proc (:include proc))
  oe-vars)				; list of vars in oe

(defstruct var-info
  requireds
  optionals
  keys
  rest-var
  restv-var
  allow-other-keys?
  auxes
  hairy?
  all-vars)

(defstruct basic-optional
  var
  init-form-expression	      
  init-form
  supplied-var)

(defstruct (optional (:include basic-optional)))
  
(defstruct (key (:include basic-optional))
  name)					

(defstruct var				; names functions and variables
  name
  c-name				; init var or func name
  declared-ok-to-ignore?
  (definite-type t)
  possible-types
  num-refs
  num-defs
  extent				; hey! bad name, change it....
  dynamic-extent?			; t when program declares it
  innermost-proc)			; only for indefinite

(defstruct (function-var (:include var)))

(defstruct (variable-var (:include var)))

(defstruct (var-op (:include code))
  var
  innermost-proc)

(defstruct (var-ref (:include var-op)))

(defstruct (var-def (:include var-op))
  value)

(defstruct (constant (:include code))
  data)

(defstruct (branch (:include code))
  inline-test?
  test)

(defstruct (if (:include branch))
  then
  else)

(defstruct (switch (:include branch))
  keys
  consequents
  default)

(defstruct (inline-mv-call (:include values-seq))
  new-holder
  var-info)

(defstruct (mvalues (:include code))
  args)

(defstruct (named-local (:include values-seq)) ; LET, FLET, LABELS
  vars
  letrec?)

(defstruct (function-call (:include code))
  args
  info
  name)

(defstruct (named-call (:include function-call))
  emit-as-goto?)

(defstruct (foreign-call (:include function-call)))

(defstruct (unnamed-call (:include function-call))
  spread-args?
  function-form)

(defstruct (primitive-call (:include function-call)))

(defstruct (c-struct-op (:include function-call))
  struct-info
  field)

(defstruct (c-struct-ref (:include c-struct-op))) ; args = (struct)

(defstruct (c-struct-def (:include c-struct-op))) ; args = (struct value)

(defvar *c-stream*)
(defvar *win-stream*)
(defvar *k-stream*)
(defvar *package-stream*)
(defvar *string-counter* -1)
(defvar *const-labels*)
(defvar *delay-proc-emit?* nil)
(defvar *tmp-var-counter*)
(defvar *name-id-counter*)
(defvar *proc-chain* nil)

(defstruct foreign-symbol
  name)

(defvar *undefined-functions* (make-hash-table :size 500 :test #'eq))

(defstruct function-info
  name
  ins
  outs
  in-types
  out-types
  meta-eval-arg-types
  meta-eval-function)

(defstruct (function-and-method-info (:include function-info))
  methods)

(defstruct (proc-info (:include function-and-method-info))
  lambda-expr
  source-file
  inline?
  defined?)

(defstruct (foreign-info (:include function-and-method-info))
  foreign-name
  in-type-objects
  out-type-objects)

(defstruct (compiler-method (:include function-info))
  new-function
  transform)

(defstruct (primitive-info (:include function-info))
  emitter)

(defstruct lex-env 
  outermost-form
  decls
  variables
  functions
  blocks
  tags)

(defvar *env*)

(defvar *compiler-macro-env* (make-macro-env))

(defvar *standard-type-specifier-symbols*
  `(array               fixnum         package             simple-vector
    atom                float          pathname            single-float
    bignum              function       random-state        standard-char
    bit                 hash-table     ratio               stream
    bit-vector          integer        rational            string
    character           keyword        readtable           string-char
    common              list           sequence            symbol
    compiled-function   long-float     short-float         t
    complex             nil            simple-array        vector
    cons                null           simple-bit-vector
    double-float        member         simple-string))

(defvar *ok-foreign-declarations* nil
    "List of foreign declarations which are ok to ignore")

(defstruct decls 
  specials
  types
  ftypes
  inlines
  notinlines
  ignores
  dynamic-extents
  optimizes)

;;; HEY! Make subtypes of this, zap kind slot?
(defstruct variable-info		; HEY! call it GLOBAL-VARIABLE-INFO
  name
  kind
  (type t)
  constant-expr				; used only by constants
  ref-before-def?)

(defstruct library
  name
  directory
  version
  load-date
  lisp-files
  symbol-table
  procedure-info
  c-type-info
  proclaims
  init-thunk
  other-object-files)

(defvar *primary-function-info* nil)

(defvar *new-function-info* nil)

(defvar *variable-info* (make-hash-table :size 300))

;; HEY! These are also defined in the library. Merge...
(defvar *structure-info* (make-hash-table :size 100))

(defvar *c-named-types* (make-hash-table :size 100))

(defvar *referenced-c-info* nil)

(defvar *external-procs* nil)

(defconstant argc-var-name "argc")

(defvar *emitting-proc?* nil)

(defvar *inner-procs*)

(defvar *analysis-errors*)

(defvar *node-id*)

(defvar *dynamic-control-points*)

(defvar *break-on-compiler-warn?* nil)
  
(defvar *keyword-package* (find-package "KEYWORD"))

(defvar *compile-file-pathname*)

(defvar *compile-verbose* t)

(defvar *compile-print* nil)

(defvar *libraries* (make-hash-table :test #'equal))

(defvar *compiler-initialized?* nil)

(defvar *compiler-package* *lisp-package*)

(defvar *line-number-readtable* (make-line-number-readtable))

(defvar *source-table* nil)

(defvar *link-start-time*)

(defvar *char-conversions*
  #("_00" "_01" "_02" "_03" "_04" "_05" "_06" "_07" "_08" "_09" "_0A"
    "_0B" "_0C" "_0D" "_0E" "_0F" "_10" "_11" "_12" "_13" "_14" "_15"
    "_16" "_17" "_18" "_19" "_1A" "_1B" "_1C" "_1D" "_1E" "_1F" "_20"
    "_21" "_22" "_23" "_24" "_25" "_26" "_27" "_28" "_29" "_2A" "_2B"
    "_2C" "_2D" "_2E" "_2F" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
    "_3A" "_3B" "_3C" "_3D" "_3E" "_3F" "_40" "A" "B" "C" "D" "E" "F"
    "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W"
    "X" "Y" "Z" "_5B" "_5C" "_5D" "_5E" "_5F" "_60" "a" "b" "c" "d" "e" "f"
    "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x"
    "y" "z" "_7B" "_7C" "_7D" "_7E" "_7F"))

(defstruct (application-package
	     (:print-function
	      (lambda (x stream depth)
		(declare (ignore depth))
		(format stream "#<APPLICATION PACKAGE: ~A>"
			(application-package-host-package x)))))
  host-package
  symbol-array-c-name
  symbols)

(defstruct application-symbol
  sym
  used-as-data?
  value
  plist		
  (c-name (lisp->c-symbol-name sym))
  function
  (hash-code (sxhash sym))
  (flags 0))

(defvar *tmp-file-counter* 0)

(defvar *default-libraries* '(:cl))

(defvar *pic?* t)

(defvar *emit-symbol-data-function* nil)

(defstruct c-type-info
  name
  c-type
  convert-to-lisp
  convert-to-c
  constant-to-c)

(defstruct c-constant-type-info
  type)

(defstruct c-array-info			; pointer = array in C
  element-type
  dimensions)

(defstruct c-struct-info
  name
  slots)

(defstruct c-struct-slot
  name
  (c-name (lisp->c-field-name name))
  type)

(defvar c-type-int8
  (make-c-type-info :name 'int8
		    :c-type "char"
		    :convert-to-lisp "LONG_TO_FX"
		    :convert-to-c "FX_TO_LONG"
		    :constant-to-c #'identity))

(defvar c-type-uint8
  (make-c-type-info :name 'uint8
		    :c-type "unsigned char"
		    :convert-to-lisp "LONG_TO_FX"
		    :convert-to-c "FX_TO_LONG"
		    :constant-to-c #'identity))

(defvar c-type-int16
  (make-c-type-info :name 'int16
		    :c-type "short"
		    :convert-to-lisp "LONG_TO_FX"
		    :convert-to-c "FX_TO_LONG"
		    :constant-to-c #'identity))

(defvar c-type-uint16
  (make-c-type-info :name 'uint16
		    :c-type "unsigned short"
		    :convert-to-lisp "LONG_TO_FX" 
		    :convert-to-c "FX_TO_LONG"
		    :constant-to-c #'identity))


(defvar c-type-int31
  (make-c-type-info :name 'int31
		    :c-type "int"
		    :convert-to-lisp "LONG_TO_FX"
		    :convert-to-c  "FX_TO_LONG"
		    :constant-to-c #'identity))
		    

(defvar c-type-uint31
  (make-c-type-info :name 'uint31
		    :c-type "unsigned int"
		    :convert-to-lisp "LONG_TO_FX"
		    :convert-to-c "FX_TO_LONG"
		    :constant-to-c #'identity))


(defvar c-type-int32
  (make-c-type-info :name 'int32
		    :c-type "int"
		    :convert-to-lisp "LONG_TO_INTEGER"
		    :convert-to-c "INTEGER_TO_LONG"
		    :constant-to-c #'identity))

(defvar c-type-uint32
  (make-c-type-info :name 'uint32
		    :c-type "unsigned int"
		    :convert-to-lisp "ULONG_TO_INTEGER"
		    :convert-to-c "INTEGER_TO_ULONG"
		    :constant-to-c #'identity))

(defvar c-type-long
  (make-c-type-info :name 'long
		    :c-type "long"
		    :convert-to-lisp "LONG_TO_INTEGER"
		    :convert-to-c "INTEGER_TO_LONG"
		    :constant-to-c #'identity))

(defvar c-type-ulong
  (make-c-type-info :name 'ulong
		    :c-type "unsigned long"
		    :convert-to-lisp "LONG_TO_INTEGER"
		    :convert-to-c "INTEGER_TO_LONG"
		    :constant-to-c #'identity))

(defvar c-type-char
  (make-c-type-info :name 'char
		    :c-type "char"
		    :convert-to-lisp "NEW_CHAR"
		    :convert-to-c "RAW_CHAR"))

(defvar c-type-double
  (make-c-type-info :name 'double
		    :c-type "double"
		    :convert-to-lisp "NEW_FLOAT"
		    :convert-to-c "RAW_FLOAT"))

(defvar c-type-lptr
  (make-c-type-info :name 'lptr
		    :c-type "LP"
		    :convert-to-lisp ""
		    :convert-to-c ""))

(defvar c-type-fptr
  (make-c-type-info :name 'fptr
		    :c-type ""
		    :convert-to-lisp "NEW_FPTR"
		    :convert-to-c "RAW_FPTR"))

(defvar c-type-void
  (make-c-type-info :name 'void
		    :c-type "void"
		    :convert-to-lisp ""
		    :convert-to-c ""))

;;; This is wierd, but primitives need it
(defvar c-type-if-test
  (make-c-type-info :name 'if-test
		    :c-type "if-test"
		    :convert-to-lisp ""
		    :convert-to-c ""))

(defvar c-type-char-string
  (make-c-array-info :element-type c-type-char
		     :dimensions nil))
