;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; Don't load these constants into a cross compiler!
(defconstant nil nil)
(defconstant t t)

(defconstant char-code-limit 256)

(defconstant pi 3.1415926535897931)
(defconstant most-positive-fixnum (- (expt 2 (- *target-bits-per-word* 2))
				     1))
(defconstant most-negative-fixnum (* (expt 2 (- *target-bits-per-word* 2))
				     -1))


(defconstant call-arguments-limit 512)
(defconstant lambda-parameters-limit 512)
(defconstant array-total-size-limit (- (expt 2 (- *target-bits-per-word* 8))
				       1))
(defconstant array-dimension-limit 16777215)
(defconstant array-rank-limit 1048576)

;;; Why do these start at 2?
(defvar *gensym-counter* 2)
(defvar *gensym-prefix* "G")
(defvar *gentemp-counter* 2)
(defvar *symtemp-counter* 2)
  
(defvar *character-names* '(("Space" .  #\Space)
			    ("Newline" . #\Newline)
			    ("Linefeed" . #\Linefeed)
			    ("Return" .  #\Return)
			    ("Tab" . #\Tab)
			    ("Page" . #\Page)
			    ("Null" . #\Null)
			    ("Backspace" . #\Backspace)
			    ("Rubout" . #\Rubout)
			    ("Escape" . #\Escape)
			    ("Esc" . #\Escape)))


;;; Booting stuff...
(defvar *macro-expanders* nil)
(defvar *compiler-macro-expanders* nil)
(defvar *type-macro-expanders* nil)
(defvar *setf-methods* nil)
(defvar *delay-structure-defs?* t)
(defvar *delayed-structure-defs* nil)
(defvar *empty-bucket-marker* "empty bucket marker")

;;; Structure info is stored in a hashtable, but hashtables
;;; are built out of structures. Hmmm....
(defvar *structure-info* (make-hash-table :size 100 :test #'eq))

(defstruct (hash-table (:constructor fresh-hash-table)
		       (:print-function write-hash-table))
  buckets
  test
  size
  length
  count
  rehash-size
  rehash-threshold
  rehash-limit)

(defsetf gethash set-gethash)


