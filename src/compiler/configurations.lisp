;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defvar *root-directory* nil)

(defstruct (config (:print-function print-config))
  name
  cc-debug-info?
  (cc-optimizer-level 1)
  indirect-calls?
  argc-check?
  full-type-checking?
  structure-type-checking?
  array-bounds-checking?
  inline-calls?
  remove-tail-recursion?
  fold-constants?
  beta?
  improve?
  misc-speed-hacks?
  lisp-line-numbers?)

(defun print-config (config stream depth)
  (declare (ignore depth))
  (format stream "#<config ~A>" (config-name config)))

(defparameter *default-config*
  (make-config :name "DEFAULT"
	       :cc-debug-info? t
	       :cc-optimizer-level 1
	       :argc-check? t
	       :full-type-checking? t  
	       :structure-type-checking? nil
	       :array-bounds-checking? t
	       :inline-calls? t
	       :misc-speed-hacks? t
	       :improve? t
	       :beta? t
	       :fold-constants? t
	       :remove-tail-recursion? t
	       :indirect-calls? t))

(defparameter *fastest-config*
  (make-config :name "FASTEST"
	       :cc-debug-info? t
	       :cc-optimizer-level 2
	       :argc-check? nil
	       :full-type-checking? nil
	       :structure-type-checking? nil
	       :array-bounds-checking? nil
	       :inline-calls? t
	       :misc-speed-hacks? t
	       :beta? t
	       :improve? t
	       :fold-constants? t
	       :remove-tail-recursion? t
	       :indirect-calls? t))

(defparameter *debug-library-config*
  (make-config :name "Debug Library"
	       :cc-debug-info? t
	       :cc-optimizer-level 2
	       :argc-check? t
	       :full-type-checking? t
	       :structure-type-checking? nil
	       :array-bounds-checking? nil
	       :inline-calls? t
	       :misc-speed-hacks? t
	       :improve? t
	       :beta? t
	       :fold-constants? t
	       :remove-tail-recursion? t
	       :indirect-calls? t))

(defparameter *standard-library-config*
  (make-config :name "Standard Library"
	       :cc-debug-info? nil
	       :cc-optimizer-level 2
	       :argc-check? t
	       :full-type-checking? t
	       :structure-type-checking? nil
	       :array-bounds-checking? nil
	       :inline-calls? t
	       :misc-speed-hacks? t
	       :beta? t
	       :improve? t
	       :fold-constants? t
	       :remove-tail-recursion? t
	       :indirect-calls? t))

(defparameter *delivery-library-config*
  (make-config :name "Delivery Library"
	       :cc-debug-info? nil
	       :cc-optimizer-level 2
	       :argc-check? nil
	       :full-type-checking? nil
	       :structure-type-checking? nil
	       :array-bounds-checking? nil
	       :inline-calls? t
	       :misc-speed-hacks? t
	       :beta? t
	       :improve? t
	       :fold-constants? t
	       :remove-tail-recursion? t
	       :indirect-calls? nil))

(defparameter *config* *default-config*)

(defvar *link-every-symbol?* t)

(defvar *profile?* nil)

(defstruct c-compiler
  command
  (debug-switch "-g")
  optimizer-switches
  debug-optimized?
  position-independent-code-switch)

(defparameter *gcc*
  (make-c-compiler
   :command "gcc -w -xc -pipe " ; tell gcc to compile .wcl -> .o
   :optimizer-switches #("" "-O" "-O2")
   :debug-optimized? t
   :position-independent-code-switch "-fPIC"))

(defstruct machine
   name 
   processor
   operating-system
   c-compiler
   linker-command
   shared-libraries
   link-libraries)

(defparameter *linux-gcc*
  (make-machine
   :name "PC (gcc)"
   :processor "i386"
   :operating-system "linux"
   :c-compiler *gcc*
   :linker-command "gcc -pipe"		; -n -Bdynamic for smallest image
   :shared-libraries t
   :link-libraries "-lm"))

(defvar *target-machine* nil)

(defvar *compiler-version*
  #.(library-version (lookup-library :com)))

(defvar *compiler-build-date*
  #.(with-output-to-string (x) (print-time :stream x)))

;;; System loading

(defun root-load (f)
  (print (load (format nil "~A/~A" *root-directory* f))))
	
(defun load-pprint ()
  (root-load "src/cl/pprint/package-setup")
  (root-load "src/cl/pprint/globals")
  (root-load "src/cl/pprint/interface")
  (root-load "src/cl/pprint/format-compiler")
  (root-load "src/cl/pprint/formats")
  (root-load "src/cl/pprint/initialize")
  (gc)
  (warn "Pretty Printer loaded into XP package~%"))

(defun load-clos ()
  (root-load "src/pcl/setup")
  (root-load "src/pcl/defsys")
  (warn "If you enter GDB with the message: Too many open files.")
  (warn "please type `continue'")
  (let ((pcl (find-package "PCL")))
    ;; Weirdness to get PCL running correctly
    (funcall (intern "LOAD-PCL" pcl))
    (eval `(,(intern "DEFCLASS" pcl) lisp::fixup () (x)))
    (warn "You are about to enter the debugger. Please `abort' when you do.")
    (warn "After you abort, CLOS will be loaded into the PCL package.~%")
    (gc)
    (funcall (intern "MAKE-INSTANCE" pcl) 'lisp::fixup)))

(defun load-logical-pathnames ()
  (root-load "src/misc/logical-pathnames")
  (warn "Logical Pathname functions loaded into the LISP package.~%"))



