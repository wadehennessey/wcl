;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defmethod clrhash ((table hash-table))
  (let ((buckets (hash-table-buckets table))
        (empty *empty-bucket-marker*))
    (declare (simple-vector buckets))
    (dotimes (i (hash-table-length table))
      (setf (svref buckets i) empty))
    (setf (hash-table-count table) 0))
  table)

;;; Returns 3 values: KEY VALUE FOUND?
;;; Useful when we gethash on a key which is not EQ to the corresponding
;;; key in the table. For example, the package system hashes on 
;;; on strings, but symbols are stored in the table).
(defmethod gethash-with-key (key (table hash-table) hash-code)
  (multiple-value-bind (value i) (hash-key->index/3 key table hash-code)
    (if (eq value *empty-bucket-marker*)
        (values nil nil nil)
        (values (%32bit-ref (hash-table-buckets table) i) value t))))

(defmethod gethash (key (table hash-table))
  (gethash/3 key table (sxhash key)))

(defun gethash/3 (key table hash-code)
  (multiple-value-bind (value i) (hash-key->index/3 key table hash-code)
    (declare (ignore i))
    (if (eq value *empty-bucket-marker*)
        (values nil nil)
        (values value t))))

(defun hash-key->index (key table)
  (hash-key->index/3 key table (sxhash key)))

(defun hash-key->index/3 (key table hash-code)
  (let ((start (hash/3 key table hash-code))
        (buckets (hash-table-buckets table))
        (test (hash-table-test table))
        (empty *empty-bucket-marker*)
        (length (hash-table-length table)))
    (declare (simple-vector buckets)
             (fixnum start))
    (loop for i from start below length by 2
          as x = (svref buckets i)
          when (or (funcall test x key) (eq x empty))
          do (return-from hash-key->index/3
	       (values (svref buckets (%+ i 1)) i)))
    (loop for i from 0 below start by 2
          as x = (svref buckets i)
          when (or (funcall test x key) (eq x empty))
          do (return-from hash-key->index/3
	       (values (svref buckets (%+ i 1)) i)))
    (error "Internal hashtable error")))

(defun sxhash/simple-string (s)
  (declare (simple-string s))
  (loop with len fixnum = (length s)
	for i from -1 below len
 	as h fixnum = len then (logand
				(the fixnum
				     (+ (the fixnum (* h 613))
					(the fixnum (char-code (%schar s i)))))
				hash-mask)
	finally (return h)))

(defun-inline hash/3 (key table hash-code)
  (%ashl (%rem hash-code (hash-table-size table)) 1))

(defun hash (key table)
  (hash/3 key table (sxhash key)))

(defun make-hash-table-1 (test size rehash-size rehash-threshold)
  (unless (and (floatp rehash-threshold)
               (> rehash-threshold 0.0)
               (< rehash-threshold 1.0))
    (error "Rehash-threshold ~A is not a floating-point ~
            number between 0 and 1" rehash-threshold))
  (let* ((real-size (prime-ceiling size))
	 (length (* real-size 2)))
    (fresh-hash-table
     :buckets (make-array length
                          :initial-element *empty-bucket-marker*)
     :test test
     :size real-size
     :length length
     :count 0
     :rehash-size rehash-size
     :rehash-threshold rehash-threshold
     :rehash-limit (floor (* rehash-threshold real-size)))))

;;; SPICE 
(defun prime-ceiling (x)
  (declare (fixnum x))
  (if (= (%rem x 2) 0) (setq x (%+ 1 x)))
  (if (= (%rem x 3) 0) (setq x (%+ 2 x))) 
  (if (= (%rem x 7) 0) (setq x (%+ 4 x)))
  x)

(defun-inline make-hash-table (&key (test #'eql)
				    (size 65)
				    (rehash-size 2.0)
				    (rehash-threshold .6))
  (make-hash-table-1 test size rehash-size rehash-threshold))

(defmethod maphash ((function function) (table hash-table))
  (let ((buckets (hash-table-buckets table)))
    (declare (simple-vector buckets))
    (loop for i from 0 below (hash-table-length table) by 2
	  as key = (svref buckets i)
	  unless (eq key *empty-bucket-marker*)
          do (funcall function key (svref buckets (%+ i 1)))))
  nil)

(defun rehash (table)
  (let* ((rehash-size (hash-table-rehash-size table))
	 (size (hash-table-size table))
	 (new-size (prime-ceiling (etypecase rehash-size
				    (float (ceiling (* rehash-size size)))
				    (fixnum (+ rehash-size size)))))
	 (new-length (%* new-size 2))
	 (new-buckets (make-array new-length
				  :initial-element *empty-bucket-marker*))
	 (old-length (hash-table-length table))
	 (old-buckets (hash-table-buckets table))
	 (empty *empty-bucket-marker*))
    ;; HEY! This should execute without interrupts. Should
    ;; move the copy out of the critical section, though.
    (setf (hash-table-buckets table) new-buckets)
    (setf (hash-table-size table) new-size)
    (setf (hash-table-length table) new-length)
    (setf (hash-table-count table) 0)
    (setf (hash-table-rehash-limit table)
	  (floor (* new-size (hash-table-rehash-threshold table))))
    (loop for i from 0 below old-length by 2
	  as key = (svref old-buckets i)
          unless (eq key empty)
	  do (set-gethash key table (svref old-buckets (%+ i 1))))
    table))

(defmethod remhash (key (table hash-table))
  (multiple-value-bind (value i) (hash-key->index key table)
    (if (eq value *empty-bucket-marker*)
        nil
        (progn
          (setf (svref (hash-table-buckets table) i)
                *empty-bucket-marker*)
          (setf (svref (hash-table-buckets table) (+ i 1))
                *empty-bucket-marker*)
	  (decf (hash-table-count table))
	  t))))

(defmethod set-gethash (key (table hash-table) value)
  (multiple-value-bind (current-value i) (hash-key->index key table)
    (declare (fixnum i))
    (setf (svref (hash-table-buckets table) i) key)
    (setf (svref (hash-table-buckets table) (%+ i 1)) value)
    (when (eq current-value *empty-bucket-marker*)
      (let ((count (setf (hash-table-count table)
			 (%+ (hash-table-count table) 1))))
        (when (%>= count (hash-table-rehash-limit table))
          (rehash table))))
    value))

(defun-inline sxhash/symbol (x)
  (symbol-hash-code x))

(defun-inline sxhash/character (x)
  (char-code x))

(defun sxhash (x)
  (typecase x
    (symbol (sxhash/symbol x))
    (simple-string (sxhash/simple-string x))
    (fixnum x)
    (character (sxhash/character x))
    ;; HEY! need to account for gc relocating pointers!!!
    ;; Until then, we punt and reduce to linear search. Bletch.
    (t 0)))
