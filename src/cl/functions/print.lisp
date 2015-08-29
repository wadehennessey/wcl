;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun default-write-structure (x stream depth)
  (if *print-structure*
      (verbose-write-structure x stream)
      (format stream "#<~A ~X>"
	      (structure-type x) (object->pointer x))))

(defun fresh-line (&optional (stream *standard-output*))
  (with-valid-stream (s stream)
    (if (> (stream-column s) 0)
	(progn (terpri s) t)
	nil)))

(defun get-output-stream-string (stream)
  (let ((buffer (string-stream-output-sink stream)))
    (prog1 (replace/simple-string (make-string (string-stream-next stream))
				  buffer)
      (setf (string-stream-next stream) 0))))

(defun pprint (x &optional (stream *standard-output*))
  (terpri)
  (prog1 (write-object x stream)
    (write-char #\Space stream)))

(defun prin1-to-string (x)
  (with-output-to-string (stream)
    (prin1 x stream)))

(defun-inline prin1 (x &optional (stream *standard-output*))
  (prin1/2 x stream))

(defun prin1/2 (x stream)
  (let ((*print-escape* t))
    (write-object x stream)))

(defun princ-to-string (x)
  (with-output-to-string (stream)
    (princ x stream)))

(defun princ (x &optional (stream *standard-output*))
  (let ((*print-escape* nil))
    (write-object x stream)))

(defun print (x &optional (stream *standard-output*))
  (print/2 x stream))

(defun print/2 (x stream)
  (terpri stream)
  (prog1 (prin1/2 x stream)
    (write-char #\Space stream)))

(defun terpri (&optional (stream *standard-output*))
  (with-valid-stream (s stream)
    (write-char #\Newline s)
    nil))

(defun verbose-write-structure (x stream)
  (let* ((type (structure-elt x 0))
       	 (info (lookup-structure-info type)))
    (format stream "#S(~S" type)
    (loop for i from 1 below (struct-info-length info)
	  for slot in (all-slots info)
	  do (format stream " ~S ~S"
		     (struct-slot-name slot) (structure-elt x i)))
    (write-char #\) stream))
  x)

(defun write-array-verbose (array stream)
  (let ((rank (array-rank array)))
    (if (= rank 1)
	(if (simple-bit-vector-p array)
	    (write-bit-vector-verbose array stream)
	    (progn (format stream "#(")
		   (loop with len = (length array)
			 for i from 0 below len
			 do (princ (aref array i) stream)
			 unless (= i (- len 1)) do (write-char #\Space stream))
		   (format stream ")")))
	(let ((dim-vector (array-dims-vector array))
	      (underlying-vector (array-underlying-vector array)))
	  (format stream "#~DA" (array-rank array))
	  (write-array-verbose-1
	   underlying-vector stream 0 rank dim-vector 0)))
    array))

(defun write-array-verbose-1 (vector stream dim-index rank
				     dim-vector vector-index)
  (if (= dim-index rank)
      (progn (princ (aref vector vector-index) stream)
	     (1+ vector-index))
      (progn (write-char #\( stream)
	     (loop for i from 0 below (aref dim-vector dim-index)
		   do (setf vector-index
			    (progn (unless (= i 0)
				     (write-char #\Space stream))
				   (write-array-verbose-1
				    vector
				    stream
				    (1+ dim-index)
				    rank
				    dim-vector
				    vector-index)))
		   finally (progn (write-char #\) stream)
				  (return vector-index))))))

(defun write-bit-vector-verbose (bit-vector stream)
  (write-string "#*" stream)
  (loop for i from 0 below (length bit-vector)
	do (write-char (if (= (sbit bit-vector i) 0) #\0 #\1)
		       stream)))

(defun write-array (x stream)
  (if *print-array*
      (write-array-verbose x stream)
      (format stream "#<~A ~A ~A ~X>"
	      (array-type x)
	      (array-element-type x)
	      (if (vectorp x)
		  (length x)
		  (loop for i from (1- (array-rank x)) downto 0
			collect (array-dimension x i)))
	      (object->pointer x))))


(defun write-complex (c stream)
  (format stream "#C(~A ~A)" (realpart c) (imagpart c)))

(defun write-cons-1 (x prefix stream)
  (write-char prefix stream)
  (write-object (car x) stream)
  (let ((cdr (cdr x)))
    (cond ((null cdr) (write-char #\) stream))
	  ((consp cdr) (write-cons-1 cdr #\Space stream))
	  (t (write-string " . " stream)
	     (write-object cdr stream)
	     (write-char #\) stream)))))

(defun write-cons (x stream)
  (write-cons-1 x #\( stream))

#|
  (if (consp (cdr x))
      (select (car x)
	('QUOTE (write-char #\')
		(write-object (second x)))
	('FUNCTION (write-string "#'")
		   (write-object (second x)))
	('BACKQUOTE (write-char #\`)
		    (write-object (second x)))
	(*COMMA* (write-char #\,)
		 (write-object (second x)))
	(*COMMA-ATSIGN* (write-string ",@")
			(write-object (second x)))
	(*COMMA-DOT* (write-string ",.")
		     (write-object (second x)))
	(t (write-cons-normally x prefix stream)))
      (write-cons-normally x prefix stream))
|#

(defun write-function (x stream)
  (format stream "#<Function ~X>" (object->pointer x)))

(defun-inline write-line (string &optional (stream *standard-output*)
				   &key start end)
  (write-line/4 string stream start end))

(defun write-object (x stream)
  (typecase x
    (fixnum (write-integer x stream))
    (character (write-character x stream))
    (symbol (write-symbol x stream))
    (cons (write-cons x stream))
    (float (write-float x stream))
    (structure (write-structure x stream))
    (compiled-function (write-function x stream))
    (bignum (write-integer x stream))
    (ratio (write-ratio x stream))
    (complex (write-complex x stream))
    (unbound-variable-marker (write-unbound-variable-marker x stream))
    (foreign-pointer (write-foreign-pointer x stream))
    ;; HEY! move some array cases to main typecase when typecase
    ;; optimizer can handle array types.
    (t (typecase x
	 (string (if *print-escape*
		     (write-string-escape x stream)
		     (write-string x stream)))
	 (simple-vector (write-simple-vector x stream))
	 (array (write-array x stream))
	 (t (write-unknown-object x stream)))))
  x) 

(defun write-foreign-pointer (x stream)
  (format stream "#<foreign-structure (~A at ~X) ~X>"
	  (foreign-pointer-type x)
	  (object->pointer (foreign-pointer-pointer x))
	  (object->pointer x)))

(defun write-unbound-variable-marker (x stream)
  (format stream "#<Unbound Variable Marker>"))

(defun write-hash-table (table stream depth)
  (declare (ignore depth))
  (format stream "#<HASH-TABLE (~D elements) ~X>"
	  (hash-table-count table)
	  (object->pointer table)))

(defun write-pathname (p stream depth)
  (let ((s (pathname->string p)))
    (if *print-escape*
	(progn (write-string "#P" stream)
	       (prin1 s stream))
	(write-object s stream))))

(defun write-random-state (state stream depth)
  (declare (ignore depth))
  (let ((*print-base* 16)
	(*print-array* t))
    (verbose-write-structure state stream)))

(defun write-ratio (r stream)
  (format stream "~A/~A" (numerator r) (denominator r)))

(defun write-simple-vector (x stream)
  (if *print-array*
      (progn (write-string "#(" stream)
	     (dotimes (i (length x))
	       (unless (= i 0) (write-char #\Space stream))
	       (prin1 (svref x i) stream))
	     (write-char #\) stream))
      (format stream "#<Simple-Vector ~D ~X>"
	      (length x)
	      (object->pointer x))))

(defun-inline write-string (string &optional (stream *standard-output*)
				   &key start end)
  (write-string/4 string stream start end))

(defun write-structure (x stream)
  (let* ((type (structure-elt x 0))
       	 (info (lookup-structure-info type)))
    (if (null info)
	(format stream "#<No structure info found for ~A>" type)
	(funcall (struct-info-print-function info) x stream 0))))

(defun-inline escaped-symbol-char? (c)
  (svref *character-attributes* (char-code c)))

(defun write-symbol (x stream)
  (let ((p (symbol-package x))
	(name (symbol-name x)))
    (unless (or (eq p *package*) (null *print-escape*))
      (cond ((eq p *keyword-package*)
	     (write-char #\: stream))
	    ((null p) (when *print-gensym*
			(write-simple-string-unsafe "#:" stream)))
	    (t (multiple-value-bind (home-type found?)
		   ;; HEY! This needs to be sure that packages are
		   ;; are initialized.
		   ;; avoid full FIND-SYMBOL if we can
		   (gethash/3 x (package-symbols p) (symbol-hash-code x))
		 (unless (and (eq home-type :external)
			      (member p (package-use-list *package*)
				      :test #'eq))
		   (multiple-value-bind (symbol type)
		       (find-symbol/3 x *package* (symbol-hash-code x))
		     (unless (and (null type) (eq symbol x))
		       (write-simple-string-unsafe (package-name p) stream)
		       (write-simple-string-unsafe
			(if (eq home-type :external) ":" "::")
			stream))))))))
    (if *print-escape*
	(write-symbol-escape x stream)
	(write-simple-string-unsafe name stream))
    x))

(defun symbol-name-needs-escaping? (s)
  (let* ((string (symbol-name s))
	 (len (length string))
	 (dots nil))
    (dotimes (i len (if (and (not (null dots))
			     (= len (length dots)))
			t
			nil))
      (let ((c (schar string i)))
	(cond ((escaped-symbol-char? c)
	       (return t))
	      ((char= c #\.)
	       (push c dots)))))))

(defun write-symbol-escape (s stream)
  (let ((name (symbol-name s)))
    (if (symbol-print-escape-flag-valid? s)
	(if (symbol-print-escape? s)
	    (progn (write-char #\| stream)
		   (if (or (find #\| name) (find #\\ name))
		       (dotimes (i (length name))
			 (let ((c (schar name i)))
			   (when (or (char= c #\\) (char= c #\|))
			     (write-char #\\ stream))
			   (write-char c stream)))
		       (write-simple-string-unsafe name stream))
		   (write-char #\| stream))
	    (write-simple-string-unsafe name stream))
	(progn (when (symbol-name-needs-escaping? s)
		 (set-symbol-print-escape s))
	       (set-symbol-print-escape-flag-valid? s)
	       (write-symbol-escape s stream)))))

(defun write-to-string-1 (x)
  (with-output-to-string (stream)
    (write-object x stream)))

(defun write-to-string (x &key
			  ((:escape *print-escape*) *print-escape*)  
			  ((:radix *print-radix*) *print-radix*)
			  ((:base *print-base*) *print-base*)
			  ((:circle *print-circle*) *print-circle*)
			  ((:pretty *print-pretty*) *print-pretty*)
			  ((:level *print-level*) *print-level*)
			  ((:length *print-length*) *print-length*)
			  ((:case *print-case*) *print-case*)
			  ((:gensym *print-gensym*) *print-gensym*)
			  ((:array *print-array*) *print-array*)
			  ((:readably *print-readably*) *print-readably*)
			  ((:right-margin *print-right-margin*)
			   *print-right-margin*)
			  ((:miser-width *print-miser-width*)
			   *print-miser-width*)
			  ((:lines *print-lines*) *print-lines*)
			  ((:pprint-dispatch *print-pprint-dispatch*)))
  (write-to-string-1 x))

(defun write-unknown-object (x stream)
  (format stream "#<Unknown object ~X>" (object->pointer x)))
 
(defun write (x &key
		(stream *standard-output*)
		((:escape *print-escape*) *print-escape*)  
		((:radix *print-radix*) *print-radix*)
		((:base *print-base*) *print-base*)
		((:circle *print-circle*) *print-circle*)
		((:pretty *print-pretty*) *print-pretty*)
		((:level *print-level*) *print-level*)
		((:length *print-length*) *print-length*)
		((:case *print-case*) *print-case*)
		((:gensym *print-gensym*) *print-gensym*)
		((:array *print-array*) *print-array*)
		((:readably *print-readably*) *print-readably*)
		((:right-margin *print-right-margin*) *print-right-margin*)
		((:miser-width *print-miser-width*) *print-miser-width*)
		((:lines *print-lines*) *print-lines*)
		((:pprint-dispatch *print-pprint-dispatch*)))
  (write-object x stream))
