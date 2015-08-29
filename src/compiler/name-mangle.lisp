;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun symbol-package-abbrev (sym)
  (package-abbrev (symbol-package sym) 0))

(defun proc-package-abbrev (sym)
  (package-abbrev (symbol-package sym) 1))

(defun macro-package-abbrev (sym)
  (package-abbrev (symbol-package sym) 2))

(defun legal-c-var-char? (char)
  (or (alpha-char-p char) (digit-char-p char)))

(defun decimal-integer-length (n)
  (loop for x = n then (floor x 10)
	counting x into len
	when (< x 10)
	return len))

(defun lisp->c-name (leader name trailer)
  (let* ((lisp-name (if (symbolp name) (symbol-name name) name))
	 (lisp-len (length lisp-name))
	 (legal-char-count (loop for c being the array-elements of lisp-name
				 when (legal-c-var-char? c) count c))
	 (leader-len (length leader))
	 (trailer-len (if (null trailer)
			  0
			  (+ (decimal-integer-length trailer) 1)))
	 (c-name-len (+ leader-len
			lisp-len
			trailer-len
			(* (- lisp-len legal-char-count)
			   2)))		; This number depends the "_~X" above
	 (c-name (make-string c-name-len)))
    (loop for i from 0 below leader-len do
	  (setf (schar c-name i) (schar leader i)))
    (loop for from from 0 below lisp-len
	  for to from leader-len below c-name-len
	  do (let ((src-char (schar lisp-name from)))
	       (if (legal-c-var-char? src-char)
		   (setf (schar c-name to) src-char)
		   (progn (replace c-name
				   (svref *char-conversions*
					  (char-code src-char))
				   :start1 to)
			  (incf to 2)))))
    (unless (null trailer)
      (setf (schar c-name (- c-name-len trailer-len)) #\_)
      (loop for i from trailer-len downto 2
	    for to from (- c-name-len 1) downto 0
	    as rest = trailer then (floor rest 10)
	    do (setf (schar c-name to)
		     (code-char (+ (char-code #\0) (rem rest 10))))))
    c-name)) 

(defun lisp->c-field-name (name)
  (lisp->c-name "" name nil))

(defun lisp->c-function-name (name id)
  (lisp->c-name "f_" name id))

(defun lisp->c-variable-name (name id)
  (lisp->c-name "v_" name id))

(defun lisp->c-block-name (name id)
  (lisp->c-name "b_" name id))

(defun lisp->c-tag-name (name id)
  (lisp->c-name "t_" name id))

(defun lisp->c-symbol-name (sym)
  (lisp->c-name (symbol-package-abbrev sym) sym nil))

(defun lisp->c-proc-name (name &optional macro-function?)
  (lisp->c-name (if macro-function?
		    (macro-package-abbrev name)
		    (proc-package-abbrev name))
		name
		nil))

