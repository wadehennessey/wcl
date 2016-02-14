;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun make-cl-library (&optional (*config* (lib-config))
				  &rest options)
  (apply #'compile-cl *config* options)
  (link-library :cl
		:predicates-file "/tmp/cl-preds.lisp"
		:data-file "/tmp/cl-data.c"
		))

(defun make-compiler-library (&optional (*config* (lib-config)))
  (compile-compiler)
  (link-library :com :other-lib-names '(:cl)
		:predicates-file "/tmp/com-preds.lisp"
		:data-file "/tmp/com-data.c"
		))

(defun make-clx-library (&optional (*config* (lib-config)))
  (compile-clx)
  (link-library :clx :other-lib-names '(:cl)
		:predicates-file "/tmp/clx-preds.lisp"
		:data-file "/tmp/clx-data.c"
		))

(defun make-eval-bin (&optional (*config* (bin-config)))
  (comf "../main/eval")
  (link-executable "../main/eval"
		   :data-file "/tmp/eval-data.c"
		   :predicates-file "/tmp/eval-preds.c"
		   :main-function 'lisp::lmain
		   :output "../../bin/eval"))

(defun make-development-bin (&optional (*config* (bin-config)))
  (comf "../main/wcl")
  (link-executable "../main/wcl"
		   :output "../../bin/wcl"
		   :lib-names '(:cl :com)
		   :main-function 'lisp::lmain))

(defun make-clx-bin (&optional (*config* (bin-config)))
  (comf "../main/clx")
  (link-executable "../main/clx"
		   :output "../../bin/clx"
		   :lib-names '(:cl :com :clx)
		   :main-function 'lisp::lmain))

(defun make-hello-bin ()
  (comf "../../test/hello")
  (link-executable "../../test/hello"
		   :main-function 'lisp::lmain))

;;; The ordering of these files is important!
(defparameter *decl-files*
  '("../cl/decls/constants"
    "../cl/decls/hash"
    "../cl/decls/macros"
    "../cl/decls/destructuring-bind"
    "../cl/decls/cl-macros"
    "../cl/decls/loop"
    "../cl/decls/defstruct"		
    "../cl/decls/foreign"		
    "../cl/decls/cl-types"
    "../cl/decls/bq"
    "../cl/decls/last"
    "../cl/decls/condition"))

(defparameter *function-files*
  '("../cl/functions/arrays.lisp"
    "../cl/functions/backquote.lisp"
    "../cl/functions/characters.lisp"
    "../cl/functions/defstruct.lisp"
    "../cl/functions/eval.lisp"
    "../cl/functions/files.lisp"
    "../cl/functions/format.lisp"
    "../cl/functions/hash.lisp"
    "../cl/functions/initialize-format.lisp"
    "../cl/functions/list.lisp"
    "../cl/functions/loop.lisp"
    "../cl/functions/low-io.lisp"
    "../cl/functions/macro-support.lisp"
    "../cl/functions/math.lisp"
    "../cl/functions/misc.lisp"
    "../cl/functions/packages.lisp"
    "../cl/functions/print.lisp"
    "../cl/functions/reader.lisp"
    "../cl/functions/sequences.lisp"
    "../cl/functions/strings.lisp"
    "../cl/functions/symbols.lisp"
    "../cl/functions/time.lisp"
    "../cl/functions/types.lisp"
    "../cl/functions/condition.lisp"
    "../cl/functions/xloop.lisp"))

(defparameter *cl-other-files*
  '("../cl/c-src/apply.c"
    "../cl/c-src/arith.c"
    "../cl/c-src/array.c"
    "../cl/c-src/cache-flush.c"
    "../cl/c-src/chars.c"
    "../cl/c-src/closure.c"
    "../cl/c-src/dynamic.c"
    "../cl/c-src/error.c"
    "../cl/c-src/eval-code.c"
    "../cl/c-src/funcall.c"
    "../cl/c-src/generic-loader.c"
    "../cl/c-src/init.c"
    "../cl/c-src/integer-arith.c"
    "../cl/c-src/interrupts.c"
    "../cl/c-src/io.c"
    "../cl/c-src/memory.c"
    "../cl/c-src/misc.c"
    "../cl/c-src/socket.c"
    "../cl/c-src/time.c"))

(defparameter *compiler-files*
  '("../compiler/globals"
    "../compiler/macros"
    "../compiler/configurations"
    "../compiler/function-info"
    "../compiler/type-dispatch"
    "../compiler/line-numbers"
    "../compiler/analyze"
    "../compiler/beta"
    "../compiler/tree-nsubst"
    "../compiler/improve"
    "../compiler/name-mangle"
    "../compiler/emit-code"
    "../compiler/emit-data"
    "../compiler/compiler-init"
    "../compiler/compile"
    "../compiler/symbol-table"
    "../compiler/library"
    "../compiler/link"))

(defparameter *clx-files*
  '("../clx/package"
    "../clx/depdefs"
    "../clx/clx"
    "../clx/dependent"		
    "../clx/macros"
    "../clx/bufmac"
    "../clx/buffer"
    "../clx/display"
    "../clx/gcontext"
    "../clx/input1"
    "../clx/input2"	
    "../clx/requests1"			
    "../clx/requests2"			
    "../clx/fonts"			
    "../clx/graphics"
    "../clx/text"
    "../clx/attributes"			
    "../clx/translate"
    "../clx/keysyms"			
    "../clx/manager"			
    "../clx/image"
    "../clx/resource"
    "../clx/patch"))

(defun compile-cl (&optional (*config* (lib-config))
			     &key
			     (rtl? t)
			     (decls? t))
  (initialize-compiler)			; setup primary function info
  (let ((*pic?* t))
    (compile-src-files *decl-files* decls? nil)
    (compile-src-files *function-files* rtl?)
    (shell (format nil
		   "cd ~A/src/cl/c-src/; make clean; make ~A"
		   *root-directory*
		   (if (= *target-bits-per-word* 64)
		       "linux-pc-64"
		       "linux-pc")))))

(defun compile-compiler (&optional (*config* (lib-config)))
  (let ((*pic?* t))
    (compile-src-files *compiler-files* t))
  t)

(defun compile-clx (&optional (*config* (lib-config))) 
  ;; Load some declarations
  (load "../clx/package.lisp")
  (load "../clx/depdefs.lisp")
  (load "../clx/clx.lisp")
  (load "../clx/dependent.lisp")
  (load "../clx/macros.lisp")
  (load "../clx/bufmac.lisp")
  (initialize-compiler)
  (let ((*pic?* t))
    (compile-src-files *clx-files* t)
    t))

(defun compile-src-files (files force? &optional load?)
  (dolist (f files)
    (let ((pathname (pathname f)))
      (when (or force? (source-newer? pathname))
	(comf pathname)
	(terpri)
	(when load? (load (merge-pathnames ".o" pathname)))))))

(defun source-newer? (f)
  (newer-file? (merge-pathnames ".lisp" f) (merge-pathnames ".o" f)))

(defun newer-file? (f1 f2)
  (and (probe-file f1)
       (probe-file f2)
       (> (file-write-date f1) (file-write-date f2))))

(define-library :cl 3.0
		:lisp-files (append *decl-files* *function-files*)
		:other-object-files *cl-other-files*)

(define-library :clx 5.0
		:lisp-files *clx-files*)

(define-library :com 3.0
		:lisp-files *compiler-files*)

(defun lib-config ()
  (symbol-value (installation-parameter "LIBRARY_CONFIG")))

(defun bin-config ()
  (symbol-value (installation-parameter "BIN_CONFIG")))



