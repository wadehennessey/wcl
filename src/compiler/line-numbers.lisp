;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

#|
1) make 2 lib switch - one for lsyms, one for just regular syms
fix data emit?      
test new eql on other numb and mixed symbol compares

fix spec binds/unbind/unwind
      case UW_SPECBIND:
	LDREF(uw_top->name,SYMBOL,value) = uw_top->value;
	POP_UW_POINT;
	break;

Zap update_var and friends?

ADD INLINEs back to symbol stuff
FIX EQ

;;; ******* Line symbol specific versions of symbol primitives.
;;; HEY! This one doesn't indirect through the link field.
(defprimitive %boundp ((sym symbol) => (flag if-test))
  (emit-c "(LDREF(~A,SYMBOL,value) != UBV_MARKER)" sym))

;;; HEY! Check that the fcell really contains a function, or
;;; rely on setters to always check this?
(defprimitive %fboundp ((sym symbol) => (flag if-test))
  (emit-c "(SYMREF(~A,function) != (LP) LREF(ubf_procedure))" sym))

(defprimitive %makunbound ((sym symbol) => ())
  (emit-c "SYMREF(~A,value) = UBV_MARKER" sym))

(defprimitive %symref ((sym t) (i int) => (v t))
  (emit-c "(LP) DEREF(((LP) LDREF(~A,SYMBOL,self_link)) + ~A * sizeof(long *))" sym i))

(defprimitive %symdef ((sym t) (i int) (y t) => ())
  (emit-c "(LP) (DEREF(((LP) LDREF(~A,SYMBOL,self_link)) + ~A * sizeof(long *)) = (LD) ~A)"
	  sym i y))

add this to c_eql
if ((x_tag == 3) && (y_tag == 3)) {
return((LDREF(x,SYMBOL,self_link) == LDREF(y,SYMBOL,self_link))
? T : NIL);
}
|#

;;; Skip whitespace and comments so line number of start of form is correct.
(defun skip-to-next-form (stream)
  (skip-to-next-form-1 stream (read-char stream nil stream)))

(defun skip-to-next-form-1 (stream char)
  (cond ((= (get-char-syntax *readtable* char) whitespace)
	 (skip-to-next-form-1 stream (read-char stream nil stream)))
	((char= char #\;)
	 (loop for ch = (read-char stream nil #\Newline t)
	       until (char= ch #\Newline))
	 (skip-to-next-form-1 stream (read-char stream nil stream)))
	((eq stream char) stream)
	(t (unread-char char stream))))

(defun char-macro-open-paren-with-lines (stream char)
  (declare (ignore char))
  (if (line-number-stream-p stream)
      (let ((*open-paren-count* (+ *open-paren-count* 1)))
	(skip-to-next-form stream)
	(let ((line (line-number-stream-line stream))
	      (l (read-list-with-lines stream)))
	  (setf (gethash l *source-table*) line)
	l))
      (char-macro-open-paren stream char)))

(defun read-list-with-lines (stream)
  (skip-to-next-form stream)
  (let* ((line (line-number-stream-line stream))
	 (x (read/4 stream t nil t))
	 (item (if (symbolp x) (make-line-symbol x line) x)))    
    (select item
      (*close-paren-marker* nil)
      (*dot-marker*
       (let ((cdr (read/4 stream t nil t))
	     (close (read/4 stream t nil t)))
	 (unless (eq close *close-paren-marker*)
	   (error "A closing parenthesis is missing after a dot"))
	 cdr))
      (t (let ((l (cons item (read-list-with-lines stream))))
	   (if (or (consp item) (fixnump item) (characterp item))
	       (setf (gethash l *source-table*) line)
	       (setf (gethash item *source-table*) line))
	   l)))))

(defun make-line-number-readtable ()
  (let ((rt (make-default-readtable)))
    (set-macro-character #\( #'char-macro-open-paren-with-lines nil rt)
    rt))

(defun source-line (form)
  (if (line-symbol-p form)
      (line-symbol-line form)
      (gethash form *source-table*)))

