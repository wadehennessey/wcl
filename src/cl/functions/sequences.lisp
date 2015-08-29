;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(eval-when (compile)
  (defmacro apply-key (key element)
    `(if ,key
      (funcall ,key ,element)
      ,element))

;;; Sorting Vectors
;;; Sorting is done with a heap sort.

;;; HEAPIFY, assuming both sons of root are heaps, percolates the root element
;;; through the sons to form a heap at root.  Root and max are zero based
;;; coordinates, but the heap algorithm only works on arrays indexed from 1
;;; through N (not 0 through N-1); This is because a root at I has sons at 2*I
;;; and 2*I+1 which does not work for a root at 0.  Because of this, boundaries,
;;; roots, and termination are computed using 1..N indexes.

  (defmacro heapify (seq vector-ref root max pred key)
    (let ((heap-root (gensym))   (heap-max (gensym))     (root-ele (gensym))
	  (root-key (gensym))    (heap-max/2 (gensym))   (heap-l-son (gensym))
	  (one-son (gensym))     (one-son-ele (gensym))  (one-son-key (gensym))
	  (r-son-ele (gensym))   (r-son-key (gensym))    (var-root (gensym)))
      `(let* ((,var-root ,root)	
	      (,heap-root (1+ ,root))
	      (,heap-max (1+ ,max))
	      (,root-ele (,vector-ref ,seq ,root))
	      (,root-key (apply-key ,key ,root-ele))
	      (,heap-max/2 (ash ,heap-max -1))) ; (floor heap-max 2)
	(declare (fixnum ,var-root ,heap-root ,heap-max ,heap-max/2))
	(loop
	 (if (> ,heap-root ,heap-max/2) (return))
	 (let* ((,heap-l-son (ash ,heap-root 1)) ; (* 2 heap-root)
		;; l-son index in seq (0..N-1) is one less than heap computation
		(,one-son (1- ,heap-l-son))
		(,one-son-ele (,vector-ref ,seq ,one-son))
		(,one-son-key (apply-key ,key ,one-son-ele)))
	   (declare (fixnum ,heap-l-son ,one-son))
	   (if (< ,heap-l-son ,heap-max)
	       ;; there is a right son.
	       (let* ((,r-son-ele (,vector-ref ,seq ,heap-l-son))
		      (,r-son-key (apply-key ,key ,r-son-ele)))
		 ;; choose the greater of the two sons.
		 (when (funcall ,pred ,one-son-key ,r-son-key)
		   (setf ,one-son ,heap-l-son)
		   (setf ,one-son-ele ,r-son-ele)
		   (setf ,one-son-key ,r-son-key))))
	   ;; if greater son is less than root, then we've formed a heap again.
	   (if (funcall ,pred ,one-son-key ,root-key) (return))
	   ;; else put greater son at root and make greater son node be the root.
	   (setf (,vector-ref ,seq ,var-root) ,one-son-ele)
	   (setf ,heap-root (1+ ,one-son)) ; one plus to be in heap coordinates.
	   (setf ,var-root ,one-son)))	; actual index into vector for root ele.
	;; now really put percolated value into heap at the appropriate root node.
	(setf (,vector-ref ,seq ,var-root) ,root-ele))))


;;; BUILD-HEAP rearranges seq elements into a heap to start heap sorting.
  (defmacro build-heap (seq type len-1 pred key)
    (let ((i (gensym)))
      `(do ((,i (floor ,len-1 2) (1- ,i)))
	((minusp ,i) ,seq)
	(declare (fixnum ,i))
	(heapify ,seq ,type ,i ,len-1 ,pred ,key))))


;;; APPLY-PRED saves us a function call sometimes.
  (defmacro apply-pred (one two pred key)
    `(if ,key
      (funcall ,pred (funcall ,key ,one)
       (funcall ,key  ,two))
      (funcall ,pred ,one ,two)))

;;; Make simple-vector and miscellaneous vector sorting functions.
  (defmacro frob-rob (fun-name vector-ref)
    `(defun ,fun-name (seq pred key)
      (let ((len-1 (1- (length (the vector seq)))))
	(declare (fixnum len-1))
	(build-heap seq ,vector-ref len-1 pred key)
	(do* ((i len-1 i-1)
	      (i-1 (1- i) (1- i-1)))
	     ((zerop i) seq)
	  (declare (fixnum i i-1))
	  (rotatef (,vector-ref seq 0) (,vector-ref seq i))
	  (heapify seq ,vector-ref 0 i-1 pred key)))))

;;; STABLE-SORT-MERGE-VECTORS* takes a source vector with subsequences,
;;;    start-1 (inclusive) ... end-1 (exclusive) and
;;;    end-1 (inclusive) ... end-2 (exclusive),
;;; and merges them into a target vector starting at index start-1.
  (defmacro stable-sort-merge-vectors* (source target start-1 end-1 end-2
						 pred key source-ref target-ref)
    (let ((i (gensym))
	  (j (gensym))
	  (target-i (gensym)))
      `(let ((,i ,start-1)
	     (,j ,end-1)		; start-2
	     (,target-i ,start-1))
	(declare (fixnum ,i ,j ,target-i))
	(loop
	 (cond ((= ,i ,end-1)
		(loop (if (= ,j ,end-2) (return))
		      (setf (,target-ref ,target ,target-i)
			    (,source-ref ,source ,j))
		      (incf ,target-i)
		      (incf ,j))
		(return))
	       ((= ,j ,end-2)
		(loop (if (= ,i ,end-1) (return))
		      (setf (,target-ref ,target ,target-i)
			    (,source-ref ,source ,i))
		      (incf ,target-i)
		      (incf ,i))
		(return))
	       ((apply-pred (,source-ref ,source ,j)
			    (,source-ref ,source ,i)
			    ,pred ,key)
		(setf (,target-ref ,target ,target-i)
		      (,source-ref ,source ,j))
		(incf ,j))
	       (t (setf (,target-ref ,target ,target-i)
			(,source-ref ,source ,i))
		  (incf ,i)))
	 (incf ,target-i)))))


;;; VECTOR-MERGE-SORT is the same algorithm used to stable sort lists, but
;;; it uses a temporary vector.  Direction determines whether we are merging
;;; into the temporary (T) or back into the given vector (NIL).

  (defmacro vector-merge-sort (vector pred key vector-ref)
    (let ((vector-len (gensym)) 		(n (gensym))
	  (direction (gensym)) 		(unsorted (gensym))
	  (start-1 (gensym)) 		(end-1 (gensym))
	  (end-2 (gensym)) 		(temp-len (gensym))
	  (i (gensym)))
      `(let ((,vector-len (length (the vector ,vector)))
	     (,n 1)			; bottom-up size of contiguous runs to be merged
	     (,direction t)		; t vector --> temp    nil temp --> vector
	     (,temp-len (length (the simple-vector *merge-sort-temp-vector*)))
	     ,unsorted			; unsorted..vector-len are the elements that need
					; to be merged for a given n
	     ,start-1)			; one n-len subsequence to be merged with the next
	(declare (simple-vector *merge-sort-temp-vector*)
	 (,(if (eq vector-ref 'svref) `simple-vector `vector)
	  ,vector)
	 (fixnum ,vector-len ,n ,temp-len ,unsorted ,start-1))
	(if (> ,vector-len ,temp-len)
	    (setf *merge-sort-temp-vector*
		  (make-array (max ,vector-len (+ ,temp-len ,temp-len)))))
	(loop
	 ;; for each n, we start taking n-runs from the start of the vector
	 (setf ,unsorted 0)
	 (loop
	  (setf ,start-1 ,unsorted)
	  (let ((,end-1 (+ ,start-1 ,n)))
	    (declare (fixnum ,end-1))
	    (cond ((< ,end-1 ,vector-len)
		   ;; there are enough elements for a second run
		   (let ((,end-2 (+ ,end-1 ,n)))
		     (declare (fixnum ,end-2))
		     (if (> ,end-2 ,vector-len) (setf ,end-2 ,vector-len))
		     (setf ,unsorted ,end-2)
		     (if ,direction
			 (stable-sort-merge-vectors*
			  ,vector *merge-sort-temp-vector*
			  ,start-1 ,end-1 ,end-2 ,pred ,key ,vector-ref svref)
			 (stable-sort-merge-vectors*
			  *merge-sort-temp-vector* ,vector
			  ,start-1 ,end-1 ,end-2 ,pred ,key svref ,vector-ref))
		     (if (= ,unsorted ,vector-len) (return))))
		  ;; if there is only one run, copy those elements to the end
		  (t (if ,direction
			 (do ((,i ,start-1 (1+ ,i)))
			     ((= ,i ,vector-len))
			   (declare (fixnum ,i))
			   (setf (svref *merge-sort-temp-vector* ,i)
				 (,vector-ref ,vector ,i)))
			 (do ((,i ,start-1 (1+ ,i)))
			     ((= ,i ,vector-len))
			   (declare (fixnum ,i))
			   (setf (,vector-ref ,vector ,i)
				 (svref *merge-sort-temp-vector* ,i))))
		     (return)))))
	 ;; If the inner loop only executed once, then there were only enough
	 ;; elements for two subsequences given n, so all the elements have
	 ;; been merged into one list.  Start-1 will have remained 0 upon exit.
	 (when (zerop ,start-1)
	   (if ,direction
	       ;; if we just merged into the temporary, copy it all back
	       ;; to the given vector.
	       (dotimes (,i ,vector-len)
		 (setf (,vector-ref ,vector ,i)
		       (svref *merge-sort-temp-vector* ,i))))
	   (return ,vector))
	 (setf ,n (ash ,n 1))		; (* 2 n)
	 (setf ,direction (not ,direction))))))


;;; MERGE-VECTORS returns a new vector which contains an interleaving
;;; of the elements of vector-1 and vector-2.  Elements from vector-2 are
;;; chosen only if they are strictly less than elements of vector-1,
;;; (pred elt-2 elt-1), as specified in the manual.

  (defmacro merge-vectors (vector-1 length-1 vector-2 length-2
				      result-vector pred key access)
    (let ((result-i (gensym))
	  (i (gensym))
	  (j (gensym)))
      `(let* ((,result-i 0)
	      (,i 0)
	      (,j 0))
	(declare (fixnum ,result-i ,i ,j))
	(loop
	 (cond ((= ,i ,length-1)
		(loop (if (= ,j ,length-2) (return))
		      (setf (,access ,result-vector ,result-i)
			    (,access ,vector-2 ,j))
		      (incf ,result-i)
		      (incf ,j))
		(return ,result-vector))
	       ((= ,j ,length-2)
		(loop (if (= ,i ,length-1) (return))
		      (setf (,access ,result-vector ,result-i)
			    (,access ,vector-1 ,i))
		      (incf ,result-i)
		      (incf ,i))
		(return ,result-vector))
	       ((apply-pred (,access ,vector-2 ,j) (,access ,vector-1 ,i)
			    ,pred ,key)
		(setf (,access ,result-vector ,result-i)
		      (,access ,vector-2 ,j))
		(incf ,j))
	       (t (setf (,access ,result-vector ,result-i)
			(,access ,vector-1 ,i))
		  (incf ,i)))
	 (incf ,result-i)))))

  (defmacro seq-dispatch (sequence list-form array-form)
    `(etypecase ,sequence
      (list ,list-form)
      (vector ,array-form)))

  (defmacro vector-locater-macro (sequence body-form return-type)
    `(let ((incrementer (if from-end -1 1))
	   (start (if from-end (1- (the fixnum end)) start))
	   (end (if from-end (1- (the fixnum start)) end)))
      (declare (fixnum start end incrementer))
      (do ((index start (+ index incrementer))
	   ,@(case return-type (:position nil) (:element '(current))))
	  ((= index end) ())
	(declare (fixnum index))
	,@(case return-type
	    (:position nil)
	    (:element `((setf current (aref ,sequence index)))))
	,body-form)))

  (defmacro locater-test-not (item sequence seq-type return-type)
    (let ((seq-ref (case return-type
		     (:position
		      (case seq-type
			(:vector `(aref ,sequence index))
			(:list `(pop ,sequence))))
		     (:element 'current)))
	  (return (case return-type
		    (:position 'index)
		    (:element 'current))))
      `(if test-not
	(if (not (funcall test-not ,item (apply-key key ,seq-ref)))
	    (return ,return))
	(if (funcall test ,item (apply-key key ,seq-ref))
	    (return ,return)))))

  (defmacro vector-locater (item sequence return-type)
    `(vector-locater-macro ,sequence
      (locater-test-not ,item ,sequence :vector ,return-type)
      ,return-type))

  (defmacro locater-if-test (test sequence seq-type return-type sense)
    (let ((seq-ref (case return-type
		     (:position
		      (case seq-type
			(:vector `(aref ,sequence index))
			(:list `(pop ,sequence))))
		     (:element 'current)))
	  (return (case return-type
		    (:position 'index)
		    (:element 'current))))
      (if sense
	  `(if (funcall ,test (apply-key key ,seq-ref))
	    (return ,return))
	  `(if (not (funcall ,test (apply-key key ,seq-ref)))
	    (return ,return)))))

  (defmacro vector-locater-if-macro (test sequence return-type sense)
    `(vector-locater-macro ,sequence
      (locater-if-test ,test ,sequence :vector ,return-type ,sense)
      ,return-type))

  (defmacro vector-locater-if (test sequence return-type)
    `(vector-locater-if-macro ,test ,sequence ,return-type t))

  (defmacro vector-locater-if-not (test sequence return-type)
    `(vector-locater-if-macro ,test ,sequence ,return-type nil))


  (defmacro list-locater-macro (sequence body-form return-type)
    `(if from-end
      (do ((sequence (nthcdr (- (the fixnum (length sequence))
				(the fixnum end))
			     (reverse (the list ,sequence))))
	   (index (1- (the fixnum end)) (1- index))
	   (terminus (1- (the fixnum start)))
	   ,@(case return-type (:position nil) (:element '(current))))
	  ((or (= index terminus) (null sequence)) ())
	(declare (fixnum index terminus))
	,@(case return-type
	    (:position nil)
	    (:element `((setf current (pop ,sequence)))))
	,body-form)
      (do ((sequence (nthcdr start ,sequence))
	   (index start (1+ index))
	   ,@(case return-type (:position nil) (:element '(current))))
	  ((or (= index (the fixnum end)) (null sequence)) ())
	(declare (fixnum index))
	,@(case return-type
	    (:position nil)
	    (:element `((setf current (pop ,sequence)))))
	,body-form)))

  (defmacro list-locater (item sequence return-type)
    `(list-locater-macro ,sequence
      (locater-test-not ,item ,sequence :list ,return-type)
      ,return-type))

  (defmacro list-locater-if-macro (test sequence return-type sense)
    `(list-locater-macro ,sequence
      (locater-if-test ,test ,sequence :list ,return-type ,sense)
      ,return-type))

  (defmacro list-locater-if (test sequence return-type)
    `(list-locater-if-macro ,test ,sequence ,return-type t))

  (defmacro list-locater-if-not (test sequence return-type)
    `(list-locater-if-macro ,test ,sequence ,return-type nil))

 
  ;; Compare two elements and return if they don't match:

  (defmacro compare-elements (elt1 elt2)
    `(if test-not
      (if (funcall test-not (apply-key key ,elt1) (apply-key key ,elt2))
	  (return nil)
	  t)
      (if (not (funcall test (apply-key key ,elt1) (apply-key key ,elt2)))
	  (return nil)
	  t)))

  (defmacro search-compare-list-list (main sub)
    `(do ((main ,main (cdr main))
	  (jndex start1 (1+ jndex))
	  (sub (nthcdr start1 ,sub) (cdr sub)))
      ((or (null main) (null sub) (= (the fixnum end1) jndex))
       t)
      (declare (fixnum jndex))
      (compare-elements (car main) (car sub))))

  (defmacro search-compare-list-vector (main sub)
    `(do ((main ,main (cdr main))
	  (index start1 (1+ index)))
      ((or (null main) (= index (the fixnum end1))) t)
      (declare (fixnum index))
      (compare-elements (car main) (aref ,sub index))))

  (defmacro search-compare-vector-list (main sub index)
    `(do ((sub (nthcdr start1 ,sub) (cdr sub))
	  (jndex start1 (1+ jndex))
	  (index ,index (1+ index)))
      ((or (= (the fixnum end1) jndex) (null sub)) t)
      (declare (fixnum jndex index))
      (compare-elements (aref ,main index) (car sub))))

  (defmacro search-compare-vector-vector (main sub index)
    `(do ((index ,index (1+ index))
	  (sub-index start1 (1+ sub-index)))
      ((= sub-index (the fixnum end1)) t)
      (declare (fixnum sub-index index))
      (compare-elements (aref ,main index) (aref ,sub sub-index))))

  (defmacro search-compare (main-type main sub index)
    (if (eq main-type 'list)
	`(seq-dispatch ,sub
	  (search-compare-list-list ,main ,sub)
	  (search-compare-list-vector ,main ,sub))
	`(seq-dispatch ,sub
	  (search-compare-vector-list ,main ,sub ,index)
	  (search-compare-vector-vector ,main ,sub ,index))))

  (defmacro list-search (main sub)
    `(do ((main (nthcdr start2 ,main) (cdr main))
	  (index2 start2 (1+ index2))
	  (terminus (- (the fixnum end2)
		       (the fixnum (- (the fixnum end1)
				      (the fixnum start1)))))
	  (last-match ()))
      ((> index2 terminus) last-match)
      (declare (fixnum index2 terminus))
      (if (search-compare list main ,sub index2)
	  (if from-end
	      (setq last-match index2)
	      (return index2)))))


  (defmacro vector-search (main sub)
    `(do ((index2 start2 (1+ index2))
	  (terminus (- (the fixnum end2)
		       (the fixnum (- (the fixnum end1)
				      (the fixnum start1)))))
	  (last-match ()))
      ((> index2 terminus) last-match)
      (declare (fixnum index2 terminus))
      (if (search-compare vector ,main ,sub index2)
	  (if from-end
	      (setq last-match index2)
	      (return index2)))))


  (defmacro vector-count (item sequence)
    `(do ((index start (1+ index))
	  (count 0))
      ((= index (the fixnum end)) count)
      (declare (fixnum index count))
      (if test-not
	  (if (funcall test-not ,item (apply-key key (aref ,sequence index)))
	      (setq count (1+ count)))
	  (if (funcall test ,item (apply-key key (aref ,sequence index)))
	      (setq count (1+ count))))))

  (defmacro list-count (item sequence)
    `(do ((sequence (nthcdr start ,sequence))
	  (index start (1+ index))
	  (count 0))
      ((or (= index (the fixnum end)) (null sequence)) count)
      (declare (fixnum index count))
      (if test-not
	  (if (funcall test-not ,item (apply-key key (pop sequence)))
	      (setq count (1+ count)))
	  (if (funcall test ,item (apply-key key (pop sequence)))
	      (setq count (1+ count))))))


  (defmacro vector-fill (sequence item start end)
    `(do ((index ,start (1+ index)))
      ((= index (the fixnum ,end)) ,sequence)
      (declare (fixnum index))
      (setf (aref ,sequence index) ,item)))

  (defmacro list-fill (sequence item start end)
    `(do ((current (nthcdr ,start ,sequence) (cdr current))
	  (index ,start (1+ index)))
      ((or (atom current) (and end (= index (the fixnum ,end))))
       sequence)
      (declare (fixnum index))
      (rplaca current ,item)))

  (defmacro elt-slice (sequences n)
    `(mapcar #'(lambda (seq) (elt seq ,n)) ,sequences))

  (defmacro defquantifier (name every-result abort-sense abort-value)
    `(defun ,name (predicate first-sequence &rest more-sequences)
      (do ((seqs more-sequences (cdr seqs))
	   (length (length first-sequence))
	   (sequences (cons first-sequence more-sequences)))
	  ((null seqs)
	   (do ((index 0 (1+ index)))
	       ((= index length) ,every-result)
	     (declare (fixnum index))
	     (let ((result (apply predicate (elt-slice sequences index))))
	       (if ,(if abort-sense 'result '(not result))
		   (return ,abort-value)))))
	(declare (fixnum length))
	(let ((this (length (car seqs))))
	  (declare (fixnum this))
	  (if (< this length) (setq length this))))))

  (defmacro map-to-list (function sequences)
    `(do ((seqs more-sequences (cdr seqs))
	  (min-length (length first-sequence)))
      ((null seqs)
       (let ((result (list nil)))
	 (do ((index 0 (1+ index))
	      (splice result))
	     ((= index min-length) (cdr result))
	   (declare (fixnum index))
	   (setq splice
		 (cdr (rplacd splice
			      (list (apply ,function (elt-slice ,sequences
								index)))))))))
      (declare (fixnum min-length))
      (let ((length (length (car seqs))))
	(declare (fixnum length))
	(if (< length min-length)
	    (setq min-length length)))))

  (defmacro map-to-simple (output-type-spec function sequences)
    `(do ((seqs more-sequences (cdr seqs))
	  (min-length (length first-sequence)))
      ((null seqs)
       (do ((index 0 (1+ index))
	    (result (make-sequence ,output-type-spec min-length)))
	   ((= index min-length) result)
	 (declare (fixnum index))
	 (setf (aref result index)
	       (apply ,function (elt-slice ,sequences index)))))
      (declare (fixnum min-length))
      (let ((length (length (car seqs))))
	(declare (fixnum length))
	(if (< length min-length)
	    (setq min-length length)))))

  (defmacro map-for-effect (function sequences)
    `(do ((seqs more-sequences (cdr seqs))
	  (min-length (length first-sequence)))
      ((null seqs)
       (do ((index 0 (1+ index)))
	   ((= index min-length) nil)
	 (apply ,function (elt-slice ,sequences index))))
      (declare (fixnum min-length))
      (let ((length (length (car seqs))))
	(declare (fixnum length))
	(if (< length min-length)
	    (setq min-length length)))))

;;; If we are copying around in the same vector, be careful not to copy the
;;; same elements over repeatedly.  We do this by copying backwards.
  (defmacro mumble-replace-from-mumble ()
    `(if (and (eq target-sequence source-sequence) (> target-start source-start))
      (let ((nelts (min (- target-end target-start) (- source-end source-start))))
	(do ((target-index (+ (the fixnum target-start) (the fixnum nelts) -1)
			   (1- target-index))
	     (source-index (+ (the fixnum source-start) (the fixnum nelts) -1)
			   (1- source-index)))
	    ((= target-index (the fixnum (1- target-start))) target-sequence)
	  (declare (fixnum target-index source-index))
	  (setf (aref target-sequence target-index)
		(aref source-sequence source-index))))
      (do ((target-index target-start (1+ target-index))
	   (source-index source-start (1+ source-index)))
	  ((or (= target-index (the fixnum target-end))
	       (= source-index (the fixnum source-end)))
	   target-sequence)
	(declare (fixnum target-index source-index))
	(setf (aref target-sequence target-index)
	      (aref source-sequence source-index)))))

  (defmacro list-replace-from-list ()
    `(if (and (eq target-sequence source-sequence) (> target-start source-start))
      (let ((new-elts (subseq source-sequence source-start
			      (+ (the fixnum source-start)
				 (the fixnum
				      (min (- (the fixnum target-end)
					      (the fixnum target-start))
					   (- (the fixnum source-end)
					      (the fixnum source-start))))))))
	(do ((n new-elts (cdr n))
	     (o (nthcdr target-start target-sequence) (cdr o)))
	    ((null n) target-sequence)
	  (rplaca o (car n))))
      (do ((target-index target-start (1+ target-index))
	   (source-index source-start (1+ source-index))
	   (target-sequence-ref (nthcdr target-start target-sequence)
				(cdr target-sequence-ref))
	   (source-sequence-ref (nthcdr source-start source-sequence)
				(cdr source-sequence-ref)))
	  ((or (= target-index (the fixnum target-end))
	       (= source-index (the fixnum source-end))
	       (null target-sequence-ref) (null source-sequence-ref))
	   target-sequence)
	(declare (fixnum target-index source-index))
	(rplaca target-sequence-ref (car source-sequence-ref)))))
  
  (defmacro list-replace-from-mumble ()
    `(do ((target-index target-start (1+ target-index))
	  (source-index source-start (1+ source-index))
	  (target-sequence-ref (nthcdr target-start target-sequence)
	   (cdr target-sequence-ref)))
      ((or (= target-index (the fixnum target-end))
	(= source-index (the fixnum source-end))
	(null target-sequence-ref))
       target-sequence)
      (declare (fixnum source-index target-index))
      (rplaca target-sequence-ref (aref source-sequence source-index))))

  (defmacro mumble-replace-from-list ()
    `(do ((target-index target-start (1+ target-index))
	  (source-index source-start (1+ source-index))
	  (source-sequence (nthcdr source-start source-sequence)
	   (cdr source-sequence)))
      ((or (= target-index (the fixnum target-end))
	(= source-index (the fixnum source-end))
	(null source-sequence))
       target-sequence)
      (declare (fixnum target-index source-index))
      (setf (aref target-sequence target-index) (car source-sequence))))

  )


(defun complement/2 (function)
  #'(lambda (x y) (not (funcall function x y))))

(defun complement (function)
  #'(lambda (&rest args)
      (declare (dynamic-extent args))
      (not (apply function args))))

(defun concat-to-string-1 (new next args i end)
  (if (= i end)
      new
      (let ((seq (svref args i)))
	(loop for v being the elements of seq using (index x)
	      do (setf (schar new (+ next x)) v)
	      finally
	      (return (concat-to-string-1 new
					  (+ next (length seq))
					  args
					  (+ i 1)
					  end))))))

(defun concat-to-string (args)
  (let* ((len (loop for v being the elements of args
		    summing (length v)))
	 (new (make-string len)))
    (concat-to-string-1 new 0 args 0 (length args))))

;; HEY! FIX THIS to work with more types, complex strings, etc.
(defun concatenate (seq-type &restv args)
  ;; HEY! use type-macroexpand here, etc. 
  (ecase seq-type
    ((simple-string string) (concat-to-string args))
    ((simple-vector vector) (concat-to-vector args))))

(defun concat-to-vector (args)
  (let* ((len (loop for v being the elements of args summing (length v)))
	 (new (make-array len)))
    (concat-to-vector-1 new 0 args 0 (length args))))

(defun concat-to-vector-1 (new next args i end)
  (if (= i end)
      new
      (let ((seq (svref args i)))
	(loop for v being the elements of seq using (index x)
	      do (setf (svref new (+ next x)) v)
	      finally
	      (return (concat-to-vector-1 new
					  (+ next (length seq))
					  args
					  (+ i 1)
					  end))))))

(defun copy-seq (seq)
  (etypecase seq
    (vector (copy-vector seq))
    (list (copy-list seq))))

(defun count-if (test sequence)
  (loop for x being the elements of sequence using (index i)
	when (funcall test x)
	counting x))

;;; SPICE code
(defun count (item sequence &key from-end (test #'eql) test-not (start 0)
		   end key)
  (when (null end) (setf end (length sequence)))
  (etypecase sequence
    (list (list-count item sequence))
    (vector (vector-count item sequence))))

(defun delete/8 (item sequence from-end test start end count key)
  (etypecase sequence
    (list (if from-end
	      (delete-list-backward item sequence test start end count)
	      (delete-list-forward item sequence test start end count)))
    (vector (if from-end
		(delete-vector-backward item sequence test start end count)
		(delete-vector-forward
		 item sequence test key start end count)))))


;;; SPICE!
(defun list-delete-duplicates* (list test test-not key from-end start end)
  (declare (fixnum start))
  (let ((handle (cons nil list)))
    (do ((current (nthcdr start list) (cdr current))
	 (previous (nthcdr start handle))
	 (index start (1+ index)))
	((or (and end (= index (the fixnum end))) (null current))
	 (cdr handle))
      (declare (fixnum index))
      (if (do ((x (if from-end 
		      (nthcdr (1+ start) handle)
		      (cdr current))
		  (cdr x))
	       (i (1+ index) (1+ i)))
	      ((or (null x)
		   (and (not from-end) end (= i (the fixnum end)))
		   (eq x current))
	       nil)
	    (declare (fixnum i))
	    (if (if test-not
		    (not (funcall test-not 
				  (apply-key key (car current))
				  (apply-key key (car x))))
		    (funcall test 
			     (apply-key key (car current)) 
			     (apply-key key (car x))))
		(return t)))
	  (rplacd previous (cdr current))
	  (setq previous (cdr previous))))))


(defun vector-delete-duplicates* (vector test test-not key from-end start end 
					 &optional (length (length vector)))
  (declare (vector vector) (fixnum start length))
  (when (null end) (setf end (length vector)))
  (do ((index start (1+ index))
       (jndex start))
      ((= index end)
       (do ((index index (1+ index))		; copy the rest of the vector
	    (jndex jndex (1+ jndex)))
	   ((= index length)
	    (shrink-vector vector jndex)
	    vector)
	 (setf (aref vector jndex) (aref vector index))))
    (declare (fixnum index jndex))
    (setf (aref vector jndex) (aref vector index))
    (unless (position (apply-key key (aref vector index)) vector :key key
		      :start (if from-end start (1+ index)) :test test
		      :end (if from-end jndex end) :test-not test-not)
      (setq jndex (1+ jndex)))))

(defun delete-duplicates (sequence &key (test #'eql) test-not (start 0) from-end
				   end key)
  (etypecase sequence
    (list (list-delete-duplicates*
	   sequence test test-not key from-end start end))
    (vector (vector-delete-duplicates*
	     sequence test test-not key from-end start end))))

(defun delete-list-backward (item list test start end count)
  (error "write me"))

;;; SPICE
(defun delete-list-forward (item list test start end count)
  (let ((handle (cons nil list)))
    (do ((current (nthcdr start list) (cdr current))
	 (previous (nthcdr start handle))
	 (index start (1+ index))
	 (number-zapped 0))
	((or (= index (the fixnum end))
	     (= number-zapped (the fixnum count)))
	 (cdr handle))
      (declare (fixnum index number-zapped))
      (if (funcall test item (car current))
	  (progn (rplacd previous (cdr current))
		 (setq number-zapped (1+ number-zapped)))
	  (setq previous (cdr previous))))))

(defun delete-vector-backward (item vector test start end count)
  (error "write me"))

;;; HEY! This should destructively slide stuff towards the front
;;; of the vector, then do a destructive shrink vector.
(defun delete-vector-forward (item vector test key start end count)
  (loop with len = (length vector)
	with result = (make-similar-vector vector len)
	for i from (- start 1) below end
	for j = 0 then (let ((e (aref vector i)))
			 (if (funcall test item (funcall key e))
			     j
			     (progn (setf (aref result j) e)
				    (1+ j))))
	finally (return (shrink-vector result j))))

(defun-inline delete (item sequence
			   &key from-end (test #'eql) test-not
			   (start 0) (end (length sequence))
			   (count most-positive-fixnum)
			   (key #'identity))
  (delete/8
   item sequence from-end (seq-test test test-not) start end count key))

(defun-inline elt/list (s i)
  (nth i s))

(defun elt (s i)
  (etypecase s
    (list (nth i s))
    (vector (aref s i))))

 ;;; SPICE!
(defquantifier every t nil nil)

;;; SPICE CODE!
(defun list-fill* (sequence item start end)
  (declare (list sequence))
  (list-fill sequence item start end))

(defun vector-fill* (sequence item start end)
  (declare (vector sequence))
  (when (null end) (setq end (length sequence)))
  (vector-fill sequence item start end))

;;; FILL cannot default end to the length of sequence since it is not
;;; an error to supply nil for its value.  We must test for end being nil
;;; in the body of the function, and this is actually done in the support
;;; routines for other reasons (see above).
(defun fill (sequence item &key (start 0) end)
  (typecase sequence
    (list (list-fill* sequence item start end))
    (vector (vector-fill* sequence item start end))))


(defun find/7 (item sequence test key start end from-end)
  (if (null from-end)
      (loop for i from start below end
	    when (funcall test item (funcall key (elt sequence i)))
	    return (elt sequence i)
	    finally (return nil))
      (loop for i from (1- end) downto start
	    when (funcall test item (funcall key (elt sequence i)))
	    return (elt sequence i)
	    finally (return nil))))

(defun find-eq/simple-vector (x v)
  (dotimes (i (true-vector-length v) nil)
    (when (eq (svref v i) x)
      (return x))))


(defun find-if (test sequence)
  (loop for x being the elements of sequence using (index i)
	when (funcall test x) return i
	finally (return nil)))


;;; HEY! add FROM-END 
(defun find (item sequence
		  &key (test #'eql)
		  test-not
		  (start 0)
		  (end (length sequence))
		  (key #'identity)
		  from-end)
  (find/7 item
	  sequence
	  (if (null test)
	      (if (null test-not)
		  #'eql
		  #'(lambda (x y) (not (funcall test-not x y))))
	      test)
	  key
	  start
	  end
	  from-end))

(defun length (x)
  (etypecase x
    (list (length/list x))
    (vector (vector-length x))))

(defun make-sequence (type length &key (initial-element *unbound*))
  (let ((real-type (type-macroexpand type)))
    (if (equal real-type '(or null cons))
	(make-list length :initial-element initial-element)
	;; HEY! Check for valid type and valid initial-element in arrays
	(multiple-value-bind (element-type-tag element-size default-initial)
	    (type->element-type-tag (let ((element-type (second real-type)))
				      (if (eq element-type '*)
					  t
					  element-type)))
	  (make-simple-vector
	   length
	   element-type-tag
	   element-size
	   (if (eq initial-element *unbound*) default-initial initial-element)
	   nil)))))

(defun make-similar-sequence (seq length)
  (etypecase seq
    (list (make-list length))
    (vector (make-similar-vector length))))

;;; SPICE
;;; HEY! Use type-macroexpand here and fix type checks
(defun map (output-type-spec function first-sequence &rest more-sequences)
  (let ((sequences (cons first-sequence more-sequences)))
    (case output-type-spec
      ((nil) (map-for-effect function sequences))
      (list (map-to-list function sequences))
      ((simple-vector simple-string vector string)
       (map-to-simple output-type-spec function sequences))
      (t (error "~S: invalid output type specifier." output-type-spec)))))

;;; SPICE!
(defquantifier notany t t nil)

(defun nreverse/vector (s)
  (error "write me"))

(defun nreverse (s)
  (etypecase s
    (list (nreverse/list s))
    (vector (nreverse/vector s))))

(defun position/7 (item sequence test key start end from-end)
  (if (null from-end)
      (loop for i from start below end
	    when (funcall test item (funcall key (elt sequence i)))
	    return i
	    finally (return nil))
      (loop for i from (1- end) downto start
	    when (funcall test item (funcall key (elt sequence i)))
	    return i
	    finally (return nil))))

(defun position-if-not (test sequence
			     &key from-end (start 0) (key #'identity)
			     (end (length sequence)))
  (if (null from-end)
      (loop for i from start below end
	    unless (funcall test (funcall key (elt sequence i)))
	    return i
	    finally (return nil))
      (loop for i from (1- end) downto start
	    unless (funcall test (funcall key (elt sequence i)))
	    return i
	    finally (return nil))))
      
(defun position-if (test sequence)
  (loop for x being the elements of sequence using (index i)
	when (funcall test x) return i
	finally (return nil)))

(defun position (item sequence
		      &key
		      test test-not
		      (key  #'identity)
		      (start 0)
		      (end (length sequence))
		      from-end)
  (position/7 item
	      sequence
	      (if (null test)
		  (if (null test-not)
		      #'eql
		      #'(lambda (x y) (not (funcall test-not x y))))
		  #'eql)
	      key
	      start
	      end
	      from-end))

(defun remove-duplicates/list (list test key)
  (loop for rest on list
	unless (member/4 (car rest) (cdr rest) test key)
	collect (car rest)))

(defun remove-duplicates/vector (vector test key)
  (warn "Fix remove-duplicates for vector ~S" vector)
  (return-from remove-duplicates/vector vector)
  (loop with len = (length vector)
	with result = (make-similar-vector vector len)
	for i from -1 below len
	for j = 0 then (let ((e (aref vector i)))
			 (setf (aref result j) e))
	finally (return (shrink-vector result j))))

(defun remove-duplicates (sequence &key (test #'eql) test-not
				   (start 0) from-end end (key #'identity))
  (etypecase sequence
    (vector (remove-duplicates/vector sequence test key))
    (list (remove-duplicates/list sequence test key))))

(defun remove-if/list (test list key negate)
  (loop for x in list
	unless (funcall negate (funcall test (funcall key x)))
	collect x))
	
(defun remove-if-not (predicate sequence &key from-end (start 0)
			    end (count most-positive-fixnum) (key #'identity))
  (etypecase sequence
    (vector (remove-if/vector predicate sequence key #'not))
    (list (remove-if/list predicate sequence key #'not))))


(defun remove-if/vector (test vector key negate)
  (loop with len = (length vector)
	with result = (make-similar-vector vector len)
	for i from -1 below len
	for j = 0 then (let ((e (aref vector i)))
			 (if (funcall negate (funcall test (funcall key e)))
			     j
			     (progn (setf (aref result j) e)
				    (1+ j))))
	finally (return (shrink-vector result j))))

(defun remove-if (predicate sequence &key from-end (start 0)
			    end (count most-positive-fixnum) (key #'identity))
  (etypecase sequence
    (vector (remove-if/vector predicate sequence key #'identity))
    (list (remove-if/list predicate sequence key #'identity))))

(defun remove/list (item list test key)
  (loop for x in list
	unless (funcall test item (funcall key x))
	collect x))

(defun remove/vector (item list test key)
  (error "Fix remove/vector (see remove-if/vector)"))

(defun remove (item sequence &key from-end (test #'eql) test-not (start 0)
		    end (count most-positive-fixnum) (key #'identity))
  (declare (fixnum start count))
  (etypecase sequence
    (vector (remove/vector item sequence test key))
    (list (remove/list item sequence test key))))

(defun replace/simple-string (dest src)
  (replace/simple-string-1 dest src 0 (length dest) 0 (length src)))

(defun replace/simple-string-1 (dest src start1 end1 start2 end2)
  (loop for dest-i from start1 below end1
	for src-i from start2 below end2
	do (setf (schar dest dest-i) (schar src src-i)))
  dest)

;;; REPLACE cannot default end arguments to the length of sequence since it
;;; is not an error to supply nil for their values.  We must test for ends
;;; being nil in the body of the function.
(defun-inline replace (target-sequence source-sequence &key
				       ((:start1 target-start) 0)
				       ((:end1 target-end))
				       ((:start2 source-start) 0)
				       ((:end2 source-end)))
  (replace/6 target-sequence source-sequence 
	     target-start target-end
	     source-start source-end))

(defun replace/6 (target-sequence source-sequence 
				  target-start target-end
				  source-start source-end)
  (unless target-end (setq target-end (length target-sequence)))
  (unless source-end (setq source-end (length source-sequence)))
  (etypecase target-sequence
    (list (etypecase source-sequence
	    (list (list-replace-from-list))
	    (vector (list-replace-from-mumble))))
    (vector (etypecase source-sequence
	      (list (mumble-replace-from-list))
	      (vector (mumble-replace-from-mumble))))))

(defun reverse/vector (s)
  (loop with len = (length s)
	with new = (make-array len)	; HEY! keep element-type
	for i from 0 below len
	do (setf (aref new i) (aref s (- len 1 i)))
	finally (return new)))

(defun reverse (s)
  (etypecase s
    (list (reverse/list s))
    (vector (reverse/vector s))))

;;; SPICE
(defun search (sequence1 sequence2 &key from-end (test #'eql) test-not 
			 (start1 0) end1 (start2 0) end2 key)
  (declare (fixnum start1 start2))
  (when (null end1) (setf end1 (length sequence1)))
  (when (null end2) (setf end2 (length sequence2)))
  (etypecase sequence2
    (list (list-search sequence2 sequence1))
    (vector (vector-search sequence2 sequence1))))

(defun seq-test (test test-not)
  (if test-not
      (complement test-not)
      test))

(defun-inline set-elt/list (l i value)
  (set-nth i l value))

(defun set-elt (sequence i value)
  (etypecase sequence
    (vector (setf (aref sequence i) value))
    (list (set-nth i sequence value))))

;;; SPICE!

(defquantifier some nil t result)

(frob-rob sort-vector aref)

(frob-rob sort-simple-vector svref)

(defun sort (sequence predicate &key key)
  (etypecase sequence
    (simple-vector
     (if (> (the fixnum (length (the simple-vector sequence))) 0)
	 (sort-simple-vector sequence predicate key)
	 sequence))
    (list (sort-list sequence predicate key))
    (vector (if (> (the fixnum (length sequence)) 0)
		(sort-vector sequence predicate key)
		sequence))))

(defun stable-sort (sequence predicate &key key)
  (etypecase sequence
    (simple-vector (stable-sort-simple-vector sequence predicate key))
    (list (sort-list sequence predicate key))
    (vector (stable-sort-vector sequence predicate key))))


;;; Stable Sorting Lists
;;; SORT-LIST uses a bottom up merge sort.  First a pass is made over
;;; the list grabbing one element at a time and merging it with the next one
;;; form pairs of sorted elements.  Then n is doubled, and elements are taken
;;; in runs of two, merging one run with the next to form quadruples of sorted
;;; elements.  This continues until n is large enough that the inner loop only
;;; runs for one iteration; that is, there are only two runs that can be
;;; merged, the first run starting at the beginning of the list, and the
;;; second being the remaining elements.
(defun sort-list (list pred key)
  (let ((head (cons :header list))  ; head holds on to everything
	(n 1)                       ; bottom-up size of lists to be merged
	unsorted		    ; unsorted is the remaining list to be
				    ;   broken into n size lists and merged
	list-1			    ; list-1 is one length n list to be merged
	last)			    ; last points to the last visited cell
    (declare (fixnum n))
    (loop
     ;; start collecting runs of n at the first element
     (setf unsorted (cdr head))
     ;; tack on the first merge of two n-runs to the head holder
     (setf last head)
     (let ((n-1 (1- n)))
       (declare (fixnum n-1))
       (loop
	(setf list-1 unsorted)
	(let ((temp (nthcdr n-1 list-1))
	      list-2)
	  (cond (temp
		 ;; there are enough elements for a second run
		 (setf list-2 (cdr temp))
		 (setf (cdr temp) nil)
		 (setf temp (nthcdr n-1 list-2))
		 (cond (temp
			(setf unsorted (cdr temp))
			(setf (cdr temp) nil))
		       ;; the second run goes off the end of the list
		       (t (setf unsorted nil)))
		 (multiple-value-bind (merged-head merged-last)
				      (merge-lists* list-1 list-2 pred key)
		   (setf (cdr last) merged-head)
		   (setf last merged-last))
		 (if (null unsorted) (return)))
		;; if there is only one run, then tack it on to the end
		(t (setf (cdr last) list-1)
		   (return)))))
       (setf n (ash n 1)) ; (+ n n)
       ;; If the inner loop only executed once, then there were only enough
       ;; elements for two runs given n, so all the elements have been merged
       ;; into one list.  This may waste one outer iteration to realize.
       (if (eq list-1 (cdr head))
	   (return list-1))))))


;;; MERGE-LISTS*   originally written by Jim Large.
;;; 		   modified to return a pointer to the end of the result
;;; 		      and to not cons header each time its called.
;;; It destructively merges list-1 with list-2.  In the resulting
;;; list, elements of list-2 are guaranteed to come after equal elements
;;; of list-1.
(defun merge-lists* (list-1 list-2 pred key)
  (do* ((result *merge-lists-header*)
	(P result))			; P points to last cell of result
       ((or (null list-1) (null list-2)) ; done when either list used up
	(if (null list-1)		; in which case, append the
	    (rplacd p list-2)		;   other list
	    (rplacd p list-1))
	(do ((drag p lead)
	     (lead (cdr p) (cdr lead)))
	    ((null lead)
	     (values (prog1 (cdr result) ; return the result sans header
		       (rplacd result nil)) ; (free memory, be careful)
		     drag))))		; and return pointer to last element
    (cond ((apply-pred (car list-2) (car list-1) pred key)
	   (rplacd p list-2)		; append the lesser list to last cell of
	   (setq p (cdr p))		;   result.  Note: test must bo done for
	   (pop list-2))		;   list-2 < list-1 so merge will be
	  (T (rplacd p list-1)		;   stable for list-1
	     (setq p (cdr p))
	     (pop list-1)))))


;;; Stable sorting vectors is done with the same algorithm used for lists,
;;; using a temporary vector to merge back and forth between it and the
;;; given vector to sort.
(defun stable-sort-simple-vector (vector pred key)
  (vector-merge-sort vector pred key svref))

(defun stable-sort-vector (vector pred key)
  (vector-merge-sort vector pred key aref))

(defun merge (result-type sequence1 sequence2 predicate &key key)
  "The sequences Sequence1 and Sequence2 are destructively merged into
   a sequence of type Result-Type using the Predicate to order the elements."
  (if (eq result-type 'list)
      (let ((result (merge-lists* (coerce sequence1 'list)
				  (coerce sequence2 'list)
				  predicate key)))
	result)
      (let* ((vector-1 (coerce sequence1 'vector))
	     (vector-2 (coerce sequence2 'vector))
	     (length-1 (length vector-1))
	     (length-2 (length vector-2))
	     (result (make-array (+ length-1 length-2)
				 :element-type result-type)))
	(declare (vector vector-1 vector-2)
		 (fixnum length-1 length-2))
	(if (and (simple-vector-p result)
		 (simple-vector-p vector-1)
		 (simple-vector-p vector-2))
	    (merge-vectors vector-1 length-1 vector-2 length-2
			   result predicate key svref)
	    (merge-vectors vector-1 length-1 vector-2 length-2
			   result predicate key aref)))))

(defun subseq/list (list start end)
  (loop for x in (nthcdr start list)
	for i from (- end start) above 0
	collect x))

(defun subseq/vector (vector start end)
  (let ((new (make-similar-vector vector (- end start))))
    (loop for s from start below end
	  for n from 0 below end
	  do (setf (aref new n) (aref vector s)))
    new))

(defun-inline subseq (seq start &optional (end (length seq)))
  (subseq-1 seq start end))

(defun subseq-1 (seq start end)
  (etypecase seq
    (list (subseq/list seq start end))
    (vector (subseq/vector seq start end))))

(defun substitute (new old sequence &key from-end (test #'eql) test-not
		       end (count most-positive-fixnum) key (start 0))
  (nsubstitute new old (copy-seq sequence)
	       :from-end from-end :test test :test-not test-not
               :end end :count count :key key :start start))

(defun nsubstitute (new old sequence &key from-end (test #'eql) test-not
                     end (count most-positive-fixnum) key (start 0))
  (declare (fixnum count start))
  (when (null end) (setf end (length sequence)))
  (if (listp sequence)
      (if from-end
          (nreverse (nlist-substitute*
                     new old (nreverse (the list sequence))
                     test test-not start end count key))
          (nlist-substitute* new old sequence
                             test test-not start end count key))
      (if from-end
          (nvector-substitute* new old sequence -1
                               test test-not (1- end) (1- start) count key)
          (nvector-substitute* new old sequence 1
                               test test-not start end count key))))

(defun nlist-substitute* (new old sequence test test-not start end count key)
  (declare (fixnum start count end))
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (declare (fixnum index))
    (when (if test-not
              (not (funcall test-not old (apply-key key (car list))))
              (funcall test old (apply-key key (car list))))
      (rplaca list new)
      (setq count (1- count)))))

(defun nvector-substitute* (new old sequence incrementer
				test test-not start end count key)
  (declare (fixnum start incrementer count end))
  (do ((index start (+ index incrementer)))
      ((or (= index end) (= count 0)) sequence)
    (declare (fixnum index))
    (when (if test-not
              (not (funcall test-not old (apply-key key
						    (aref sequence index))))
              (funcall test old (apply-key key (aref sequence index))))
      (setf (aref sequence index) new)
      (setq count (1- count)))))

