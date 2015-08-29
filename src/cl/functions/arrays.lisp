;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun-inline object-size (x)
  (object_size x))

(defun-inline object-header (x)
  (%pointer (%32bit-ref x -1)))

(defun-inline array-dims-vector (a)
  (%32bit-ref a array-dims-vector-offset))

(defun-inline array-underlying-vector (a)
  (%32bit-ref a indirect-array-offset))

(defun-inline array-multiplier-vector (a)
  (%32bit-ref a array-mult-vector-offset a))

;;; should be unsigned-32bit-vref
(defmethod 32bit-vref ((v 32bit-vector) (i fixnum)) 
  (vector-bounds-check v i)
  (%signed-32bit-ref v i))

(defun adjust-array (a &rest stuff)
  (warn "HEY! ADJUST-ARRAY doesn't work yet..")
  a)

(defun vector-bounds-error (v index)
  (error "The index ~D is not within the bounds [0,~D) of the vector ~S"
	 index (length v)  v))

(defun multi-array-bounds-error (a indices)
  (error "Array indices [~{~A ~}] are not within the bounds of ~S"
	 (coerce indices 'list) a))

(defun not-array-error (a vector-only?)
  (error "~A is not an array" a))

(defun aref (a &restv dims)
  (c_aref a dims))

(defun array-default-initial-element (array)
  (array-element-type-tag->default-initial-element (element-type-tag array)))

(defun array-element-type-tag->default-initial-element (type-tag)
  (select type-tag
    (element-type-bit 0)
    (element-type-char #\Null)
    (element-type-signed-8bit 0)
    (element-type-unsigned-8bit 0)
    (element-type-signed-16bit 0)
    (element-type-unsigned-16bit 0)
    (element-type-signed-32bit 0)
    (element-type-unsigned-32bit 0)
    (element-type-ptr nil)
    (element-type-float 0.0)))

(defmethod array-dimension ((a array) (dim fixnum))
  (if (vectorp a)
      (if (= dim 0)
	  (length a)
	  (error "~A is not a valid dimension for a vector"))
      (let* ((dim-vector (%32bit-ref a array-dims-vector-offset))
	     (len (length dim-vector)))
	(check-range dim 0 len)
	(aref dim-vector (- (- len 1) dim)))))

(defmethod array-dimensions ((a array))
  (error "write me!"))

(defun array-element-size (array)
  (array-element-type-tag->array-element-size (element-type-tag array)))

;;; returns element size in bits
(defun array-element-type-tag->array-element-size (type-tag)
  (select type-tag
    (element-type-bit 1)
    (element-type-signed-8bit 8)
    (element-type-unsigned-8bit 8)
    (element-type-char 8)
    (element-type-signed-16bit 16)
    (element-type-unsigned-16bit 16)
    (element-type-signed-32bit 32)
    (element-type-unsigned-32bit 32)
    (element-type-ptr 32)
    (element-type-float 64)))

(defun array-element-type (a)
  (let ((tag (element-type-tag a)))
    (select tag
      (element-type-bit 'bit)
      (element-type-signed-8bit '(signed-byte 8))
      (element-type-unsigned-8bit '(unsigned-byte 8))
      (element-type-char 'character)
      (element-type-signed-16bit '(signed-byte 16))
      (element-type-unsigned-16bit '(unsigned-byte 16))
      (element-type-signed-32bit '(signed-byte 32))
      (element-type-unsigned-32bit '(unsigned-byte 32))
      (element-type-ptr 't)
      (element-type-float 'float))))

(defmethod array-rank ((a array))
  (if (vectorp a)
      1
      (multi-array-rank a)))

(defun array-type-tag (a)
  (%tag_mask a #x7))

(defun array-type (a)
  (let ((type-tag (array-type-tag a)))
    (select type-tag
      (simple-vector-tag 'simple-vector)
      (simple-multi-array-tag 'simple-array)
      (complex-vector-tag 'vector)
      (complex-multi-array-tag 'array))))

(defun char (string index)
  (aref string index))

(defun check-range-inclusive (n low-inclusive high-inclusive)
  (if (and (numberp n)
	   (>= n low-inclusive)
	   (<= n high-inclusive))
      n
      (error "~A is not a number in the range [~D,~D]"
	     n low-inclusive high-inclusive)))

(defun check-range (n low-inclusive high-exclusive)
  (if (and (numberp n)
	   (>= n low-inclusive)
	   (< n high-exclusive))
      n
      (error "~A is not a number in the range [~D,~D)"
	     n low-inclusive high-exclusive)))

(defun copy-vector (v)
  (let* ((len (length v))
	 (new (make-similar-vector v len)))
    (dotimes (i len)
      (setf (aref new i) (aref v i)))
    new))


(defun element-type-tag (a)
  (%tag_mask a array-element-type-mask))

(defmethod-inline fill-pointer ((v vector))
  (%32bit-ref v vector-fill-pointer-offset))

(defun illegal-element-type (a element)
  (error "The element ~A cannot be stored into the array ~A" element a))

(defun initialize-array-from-contents (a underlying-vector contents)
  (let ((rank (array-rank a)))
    (initialize-array-from-contents-1 underlying-vector
				      rank
				      0
				      (if (> rank 1)
					  (array-dims-vector a)
					  (vector (length a)))
				      0
				      contents
				      contents)
    a))

(defun initialize-array-from-contents-1 (vector rank vector-index
						dims dim-index
						contents whole-contents)
  (if (= dim-index rank)
      (progn (setf (aref vector vector-index) contents)
	     (1+ vector-index))
      (if (= (length contents) (aref dims dim-index))
	  (loop for c in contents
		do (setf vector-index
			 (initialize-array-from-contents-1 vector
							   rank
							   vector-index
							   dims
							   (1+ dim-index)
							   c
							   whole-contents))
		finally (return vector-index))
	  (error "~A is not a legal initial-contents specificiation ~
                           for an array with dimensions ~A"
		 whole-contents dims))))

(defun initialize-array (a underlying-array
			   element-type-tag element-size
			   initial-element initial-contents)
  (if (null initial-contents)
      (initialize_array
       underlying-array element-type-tag element-size initial-element)
      (initialize-array-from-contents a underlying-array initial-contents)))

(defmethod make-32bit-vector ((len fixnum))
  (alloc_words len type-simple-unsigned-32bit-vector))

(defun make-array (dims &key
			(element-type t)
			(initial-element nil initial-element?)
			initial-contents
			adjustable
			fill-pointer
			displaced-to
			(displaced-index-offset 0))
  (multiple-value-bind (element-type-tag element-size default-initial)
      (type->element-type-tag element-type)
    (let ((ie (if initial-element? initial-element default-initial)))
      (if (and (null adjustable) (null fill-pointer) (null displaced-to))
	  (make-simple-array
	   dims element-type-tag element-size ie initial-contents)
	  (make-complex-array dims element-type-tag element-size ie
			      initial-contents adjustable fill-pointer 
			      displaced-to displaced-index-offset)))))

(defun make-complex-array (dims element-type-tag element-size
				initial-element initial-contents
				adjustable fill-pointer 
				displaced-to displaced-index-offset)
  (let ((vector-len (etypecase dims
		      (fixnum dims)
		      (list (if (null (cdr dims)) (car dims) nil)))))
    (if (null vector-len)
	(make-complex-multi-array dims element-type-tag element-size
				  initial-element initial-contents
				  displaced-to displaced-index-offset)
	(make-complex-vector vector-len element-type-tag element-size
			     initial-element initial-contents
			     displaced-to displaced-index-offset
			     fill-pointer))))

(defun make-complex-multi-array (dims element-type-tag element-size
				      initial-element initial-contents
				      displaced-to displaced-index-offset)
  (let* ((a (make-multi-array
	     dims element-type-tag element-size complex-multi-array-tag 4
	     displaced-to displaced-index-offset))
	 (underlying-array (%32bit-ref a indirect-array-offset)))
    (initialize-array a underlying-array element-type-tag element-size
		      initial-element initial-contents)
    a))

(defmethod make-complex-vector ((len fixnum) element-type-tag element-size
				initial-element	initial-contents displaced-to
				displaced-index-offset fill-pointer)
  (let ((underlying-array
	 (if (null displaced-to)
	     (alloc-simple-1d-array len
				    element-size
				    (+ element-type-tag
				       simple-vector-tag))
	     displaced-to))
	;; HEY! unsafe len with type field here!!!
	;; Use type-simple-vector, then bash len+type later
	(v (alloc_words 3 (+ (if (null fill-pointer) 0 #x80)
			     (+ element-type-tag
				complex-vector-tag)))))
    (%32bit-def v indirect-array-offset underlying-array)
    (%32bit-def v vector-displaced-index-offset displaced-index-offset)
    (%set-object-length v len)
    (unless (null fill-pointer)
      (setf (fill-pointer v)
	    (cond ((eq fill-pointer t) len)
		  (t (check-range-inclusive fill-pointer 0 len)))))
    (when (null displaced-to)
      (initialize-array v underlying-array element-type-tag element-size
			initial-element initial-contents))
    v))

(defun make-multi-array (dims element-type-tag element-size array-type-tag len
			      displaced-to displaced-index-offset)
  (if (null dims)
      (error "HEY! add 0 rank arrays")
      (let* ((rank (length dims))
	     (dim-vector (loop  with dim-vector = (make-32bit-vector rank)
				for i from 0 below rank
				for d in dims
				unless (fixnump d) do
				(error "Array dimension ~D is not a fixnum" d)
				do (set-32bit-vref dim-vector i d)
				finally (return dim-vector))))
	(loop with mult-vector = (make-32bit-vector rank)
	      for i from rank downto 1
	      as offset = 1 then (* offset (32bit-vref dim-vector i))
	      do (set-32bit-vref mult-vector (- i 1) offset)
	      finally
	      (return
		(let ((underlying-array
		       (if (null displaced-to)
			   (alloc-simple-1d-array
			    (* offset (car dims))
			    element-size
			    (+ element-type-tag
			       simple-vector-tag))
			   (error "HEY! Add displaced multi arrays")))
		      (a (alloc_words len
				      (+ element-type-tag
					 array-type-tag))))
		  (%32bit-def
		   a indirect-array-offset underlying-array)
		  (%32bit-def
		   a array-dims-vector-offset dim-vector)
		  (%32bit-def
		   a array-mult-vector-offset mult-vector)
		  a))))))

(defun make-similar-vector (vector length)
  (let* ((type-tag (element-type-tag vector))
	 (size (array-element-type-tag->array-element-size type-tag))
	 (initial-element 
	  (array-element-type-tag->default-initial-element type-tag)))
    ;; We downgrade complex vectors to simple vectors here.
    (make-simple-vector length type-tag size initial-element nil)))

(defun make-simple-array (dims element-type-tag element-size
			       initial-element initial-contents)
  (let ((vector-len (etypecase dims
		      (fixnum dims)
		      (list (if (null (cdr dims)) (car dims) nil)))))
    (if (null vector-len)
	(make-simple-multi-array
	 dims element-type-tag element-size initial-element initial-contents)
	(make-simple-vector
	 vector-len element-type-tag element-size
	 initial-element initial-contents))))


(defun make-simple-multi-array (dims element-type-tag element-size
				     initial-element initial-contents)
  (let* ((a (make-multi-array
	     dims element-type-tag element-size simple-multi-array-tag 3
	     nil nil))
	 (underlying-array (%32bit-ref a indirect-array-offset)))
    (initialize-array a underlying-array element-type-tag element-size
		      initial-element initial-contents)
    a))


(defun make-simple-string (size initial-element)
  (let ((s (alloc_bytes (+ size 1) type-simple-string)))
    ;; Terminating null is already there because all allocated
    ;; storage is initialized to 0.
    (%set-object-length s size)
    (unless (char= initial-element #\Null)
      (initialize_array s element-type-char 8 initial-element))
    s))

(defmethod make-simple-vector ((len fixnum) element-type-tag element-size
			       initial-element initial-contents)
  (if (%= element-type-tag element-type-char)
      ;; HEY! handle initial-contents too!
      (make-simple-string len initial-element)
      (let ((v (alloc-simple-1d-array len
				      element-size
				      (+ element-type-tag simple-vector-tag))))
	(initialize-array v v element-type-tag element-size
			  initial-element initial-contents))))

(defun alloc-simple-1d-array (len element-size tag)
  ;; If we are given a 0 length array, we must allocate at least
  ;; 1 word of storage in case the array is converted to a fowarding ptr.
  ;; The len field will still show 0, but the gc knows better (at least
  ;; now it does...)
  (let ((v (alloc_memory (if (%= 0 len) (1+ len) len)
			 element-size
			 tag)))
    (%set-object-length v len)
    v))

;;; HEY! this is a real hack to prevent infinite recursion with VECTORP
(defun multi-array-rank (a)
  (vector-length (%32bit-ref a array-dims-vector-offset)))

(defun sbit (a &restv dims)
  (c_aref a dims))

(defmethod schar ((s simple-string) (i fixnum))
  (vector-bounds-check s i)
  (%schar s i))

(defmethod set-32bit-vref ((v 32bit-vector) (i fixnum) value)
  (vector-bounds-check v i)
  (%signed-32bit-def v i value)
  value)

(defun set-aref (value a &restv dims)
  (c_set_aref value a dims))

(defmethod-inline set-fill-pointer ((v vector) (value fixnum))
  (%32bit-def v vector-fill-pointer-offset value)
  value)

(defun set-sbit (value a &restv dims)
  (c_set_aref value a dims))

(defmethod set-schar (s i value)
  (vector-bounds-check s i)
  (%set-schar s i value)
  value)

(defmethod set-svref ((v simple-vector) (i fixnum) value)
  (vector-bounds-check v i)
  (%32bit-def v i value)
  value)

;;; HEY! If we shrink by >=8 bytes, then we should alter the current
;;; vector in place, converting leftover storage to an empty string
;;; or something.
(defun shrink-vector (vector smaller-len)
  (let ((final-result (make-similar-vector vector smaller-len)))
    (loop for i from 0 below smaller-len
	  do (setf (aref final-result i) (aref vector i)))
    final-result))

(defmethod svref ((v simple-vector) (i fixnum))
  (vector-bounds-check v i)
  (%32bit-ref v i))

(defmethod-inline true-vector-length ((v vector))
  (%object-length v))

(defun-inline vector-bounds-check (v i)
  ;; Using =, etc. here will cause infinite recursion
  (unless (and (%fixnump i) (%>= i 0) (%< i (%object-length v)))
    (vector-bounds-error v i)))

(defmethod vector-length ((v vector))
  (if (vector-with-fill-pointer-p v)
      (fill-pointer v)
      (%object-length v)))

(defmethod vector-pop ((vector vector-with-fill-pointer))
  (aref vector (decf (fill-pointer vector))))

(defmethod vector-push-extend (new-element (vector vector-with-fill-pointer))
  (let* ((fp (fill-pointer vector))
	 (underlying-vector (array-underlying-vector vector))
	 (len (true-vector-length underlying-vector)))
    (when (%= fp len)
      (let* ((new-len (%* (%+ len 1) 2))
	     (new (make-similar-vector underlying-vector new-len)))
	(dotimes (i len) (setf (aref new i) (aref underlying-vector i)))
	(%32bit-def vector indirect-array-offset new)
	(%set-object-length vector new-len)))
    (setf (aref vector fp) new-element)
    (setf (fill-pointer vector) (%+ (fill-pointer vector) 1))
    fp))

(defmethod vector-push (new-element (vector vector-with-fill-pointer))
  (let ((fp (fill-pointer vector))
	(len (true-vector-length (array-underlying-vector vector))))
    (if (%= fp len)
	nil
	(progn (setf (aref vector fp) new-element)
	       (setf (fill-pointer vector) (%+ (fill-pointer vector) 1))
	       fp))))

(defun vector-ref (v i)
  (etypecase v
    (simple-vector (svref v i))
    (simple-string (schar v i))
    (32bit-vector (32bit-vref v i))
    (bit-array (error "special bit arrays not done"))))

(defun vector (&restv values)
  (let* ((len (length values))
	 (v (make-array len)))
    (loop for i from 0 below len do (setf (svref v i) (svref values i)))
    v))
