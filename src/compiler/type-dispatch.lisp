;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defconstant max-no-collision-tries 1000)
	  
(defvar *fixnum-spec* `(integer ,most-negative-fixnum ,most-positive-fixnum))

(defun duplicates? (list &optional (test #'eql))
  (loop for l1 on list
	do (loop for l2 on (cdr l1)
		 when (funcall test (car l1) (car l2))
		 do (return-from duplicates? (car l1))))
  nil)
	      
(defun no-collision-hash-size (symbols)
  (if (duplicates? symbols #'eq)
      nil
      (no-collision-hash-size-1 symbols 0 (length symbols))))

(defun no-collision-hash-size-1 (symbols num-tries size)
  (if (= num-tries max-no-collision-tries)
      nil
      (let ((bits (make-array size :element-type 'bit)))
	(or (dolist (symbol symbols size)
	      (let ((index (rem (sxhash symbol) size)))
		(if (= (sbit bits index) 0)
		    (setf (sbit bits index) 1)
		    (return nil))))
	    (no-collision-hash-size-1
	     symbols
	     (1+ num-tries)
	     (1+ size))))))

(defun find->hashed-symbol-lookup (key symbol-list)
  (let* ((no-collision-size (no-collision-hash-size symbol-list))
	 ;;; HEY! need to check if size is null, and if so punt
	 (truth-vector (make-array no-collision-size :initial-element 0))
	 (tmp (gensym "KEY")))
    (dolist (s symbol-list)
      (setf (svref truth-vector
		   (rem (sxhash s) no-collision-size))
	    s))
    `(let ((,tmp ,key))
      (eq ,tmp
       (%32bit-ref ,truth-vector
	(%rem (symbol-hash-code ,tmp) ,no-collision-size))))))

(defun optimize-case (original key-form cases)
  (multiple-value-bind (specific-cases default)
      (let ((rev (reverse cases)))
	(if (member (caar rev) '(t otherwise))
	    (values (reverse (cdr rev)) `(progn ,@(cdar rev)))
	    (values cases nil)))
    (let* ((all-keys (loop for (keys . consequent) in specific-cases
			   appending (if (listp keys) keys (list keys))))
	   (dup? (duplicates? all-keys #'equal)))
      (cond (dup? (warn "The key ~A is duplicated in ~A" dup? original))
	    ((> (length all-keys) 4)
	     (cond ((every #'symbolp all-keys)
		    (optimize-case/symbols
		     all-keys key-form specific-cases default))
		   ((every #'fixnump all-keys)
		    (optimize-case/fixnums key-form specific-cases default))
		   ((every #'characterp all-keys)
		    (optimize-case/chars key-form specific-cases default))
		   (t original)))
	    (t original)))))

(defun optimize-case/fixnums (key-form specific-cases default)
  `(switch ,key-form
    ,@(loop for (key . consequent) in specific-cases
       collect `(,key (progn ,@consequent)))
    (t ,default)))

(defun optimize-case/chars (key-form specific-cases default)
  (let ((key (gensym "KEY"))
	(default-tag (gensym "DEFAULT-TAG"))
	(name (gensym "CASE")))
    `(block ,name
      (let ((,key ,key-form))
	(tagbody
	   (when (characterp ,key)
	     (return-from ,name
	       (switch (%char-code ,key)
		 ,@(loop for (keys . consequent) in specific-cases
			 collect `(,(loop for k in (if (listp keys)
						       keys
						       (list keys))
				     collect (char-code k))
				   (progn ,@consequent)))
		 (t (go ,default-tag)))))
	   ,default-tag
	   (return-from ,name ,default))))))

(defun optimize-case/symbols (all-keys key-form specific-cases default)
  (let ((no-collision-size (no-collision-hash-size all-keys))
	;; HEY! check for nul collision size and punt
	(key (gensym "KEY"))
	(default-tag (gensym "DEFAULT-TAG"))
	(name (gensym "CASE")))
    (multiple-value-bind (flat-cases multi-consequents)
	(flatten-multi-cases specific-cases name)
      `(block ,name
	(let ((,key ,key-form))
	  (tagbody
	     (when (symbolp ,key)
	       (return-from ,name
		 (switch (%rem (symbol-hash-code ,key) ,no-collision-size)
		   ,@(loop for (sym . consequent) in flat-cases
			   collect `(,(rem (sxhash sym) no-collision-size)
				     (if (eq ,key ',sym)
					 (progn ,@consequent)
					 (go ,default-tag))))
		   (t (go ,default-tag)))))
	     ,default-tag
	     (return-from ,name ,default)
	     ,@multi-consequents))))))

(defun flatten-multi-cases (specific-cases block-name)
  (labels ((doit (rest flat-cases multi-consequents)
	     (if (null rest)
		 (values flat-cases multi-consequents)
		 (destructuring-bind (key . consequent) (car rest)
		   (if (and (listp key) (> (length key) 1))
		       (let ((tag (gensym "C")))
			 (doit (cdr rest)
			       (append (loop for k in key
					     collecting `(,k (go ,tag)))
				       flat-cases)
			       (append `(,tag
					 (return-from ,block-name
					   (progn ,@consequent)))
				       multi-consequents)))
		       (doit (cdr rest)
			     (cons (cons (if (listp key) (car key) key)
					 consequent)
				   flat-cases)
			     multi-consequents))))))
    (doit specific-cases nil nil)))
	
(defun optimize-typecase (original key-form cases)
  (multiple-value-bind (specific-cases default)
      (let ((rev (reverse cases)))
	(if (member (caar rev) '(t otherwise))
	    (values (reverse (cdr rev)) `(progn ,@(cdar rev)))
	    (values cases nil)))
    (labels
	((fail () (return-from optimize-typecase original))
	 (add-case (type-code consequent existing)
	   (cons (list type-code `(progn ,@consequent)) existing))
	 (classify (rest fixnum structure structure-cases others)
	   (if (null rest)
	       (let ((all-structs (loop for (names . consequent)
					in structure-cases
					appending names))
		     (all-others (loop for (types . consequent)
				       in others
				       appending types)))
		 (if (or (duplicates? all-others #'=)
			 (duplicates? all-structs #'eq)
			 (and (not (null structure))
			      (not (null structure-cases))))
		     
		     (fail)
		     (optimize-typecase-1 
		      key-form fixnum structure structure-cases
		      others default)))
	       (destructuring-bind (source-type . consequents) (car rest)
		 (let* ((mexp-type (type-macroexpand source-type))
			(type (if (listp mexp-type)
				  mexp-type
				  (list mexp-type)))
			(type-codes (type->type-codes type)))
		   (if (null type-codes)
		       (let ((info (lookup-structure-info (car type))))
			 (if (null info)
			     (fail)
			     (classify
			      (cdr rest)
			      fixnum
			      structure
			      (add-case (cons (car type)
					      (all-current-structure-children
					       (car type)))
					consequents
					structure-cases)
			      others)))
		       (cond ((eq type-codes :fixnum)
			      (classify (cdr rest)
					`(progn ,@consequents)
					structure
					structure-cases
					others))
			     ((eq type-codes :structure)
			      (classify
			       (cdr rest)
			       fixnum
			       (add-case type-structure consequents structure)
			       structure-cases
			       others))
			     (t (classify
				 (cdr rest)
				 fixnum
				 structure
				 structure-cases
				 (add-case
				  type-codes consequents others))))))))))
      (classify specific-cases nil nil nil nil))))

(defun type->type-codes (type)
  (case (car type)
    (integer (if (equal type *fixnum-spec*) :fixnum nil))
    ((array simple-array) (array-type->type-codes type))
    (float  (if (null (cdr type)) (list type-float) nil))
    (character (list type-character))
    (symbol (list type-symbol type-line-symbol))
    (satisfies (case (second type)
		 (bignump (list type-bignum))
		 (ratiop (list type-ratio))
		 (complexp (list type-complex))
		 (consp (list type-cons))
		 (foreign-pointer-p (list type-foreign-ptr))
		 (compiled-function-p (list type-procedure))
		 (structurep :structure)
		 (t nil)))
    (t nil)))

;;; HEY! Write this!
(defun array-type->type-codes (array-spec)
  (destructuring-bind (array-class &optional type dims) array-spec
    ;; HEY! Partially specified arrays should expand into
    ;; long lists of possible types. 
    (cond ((null dims)  nil)
	  ((null type) nil)
	  (t  array-class nil))))
       
(defun optimize-typecase-1 (key-form fixnum structure structure-cases
				     others default)
  (let ((name (gensym "TYPECASE"))
	(default-tag (gensym "DEFAULT-TAG"))
	(x (gensym "KEY")))
    `(block ,name
      (let ((,x ,key-form))
	(tagbody (return-from ,name
		   (if (fixnump ,x)
		       ,@(if (null fixnum)
			     `((go ,default-tag))
			     (list fixnum))
		       (switch (%tag ,x)
			 ,@others
			 ,@(if (null structure-cases)
			       (if (null structure)
				   nil
				   structure)
			       `((,type-structure (case (structure-type ,x)
						    ,@structure-cases
						    (t (go ,default-tag))))))
			 (t (go ,default-tag)))))
	   ,default-tag
	   (return-from ,name ,default))))))


