;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(eval-when (compile)
  (defmacro satisfies-the-test (item elt)
    `(cond (testp
	    (funcall test ,item (funcall key ,elt)))
      (notp
       (not (funcall test-not ,item (funcall key ,elt))))
      (t (funcall test ,item (funcall key ,elt)))))


  (defmacro with-set-keys (funcall)
    `(cond ((and testp notp) (error "Test and test-not both supplied."))
      (notp ,(append funcall '(:key key :test-not test-not)))
      (t ,(append funcall '(:key key :test test)))))


;;; Destination and source are setf-able and many-evaluable.  Sets the source
;;; to the cdr, and "conses" the 1st elt of source to destination.
;;;
  (defmacro steve-splice (source destination)
    `(let ((temp ,source))
      (setf ,source (cdr ,source)
       (cdr temp) ,destination
       ,destination temp)))
  )

(defun-inline cons (x y)
  (c_cons x y))

(defmethod-inline car ((x list))
  (%32bit-ref x 0))

(defmethod-inline cdr ((x list))
  (%32bit-ref x 1))

(defmethod-inline set-car ((x list) v)
  (%32bit-def x 0 v)
  v)

(defmethod-inline set-cdr ((x list) v)
  (%32bit-def x 1 v)
  v)

(defun-inline rplaca (c new-car)
  (set-car c new-car)
  c)

(defun-inline rplacd (c new-cdr)
  (set-cdr c new-cdr)
  c)

(defun-inline caaaar (c) (car (car (car (car c)))))

(defun-inline caaadr (c) (car (car (car (cdr c)))))

(defun-inline caaar (c) (car (car (car c))))

(defun-inline caadar (c) (car (car (cdr (car c)))))

(defun-inline caaddr (c) (car (car (cdr (cdr c)))))

(defun-inline caadr (c) (car (car (cdr c))))

(defun-inline caar (c) (car (car c)))

(defun-inline cadaar (c) (car (cdr (car (car c)))))

(defun-inline cadadr (c) (car (cdr (car (cdr c)))))

(defun-inline cadar (c) (car (cdr (car c))))

(defun-inline caddar (c) (car (cdr (cdr (car c)))))

(defun-inline cadddr (c) (car (cdr (cdr (cdr c)))))

(defun-inline caddr (c) (car (cdr (cdr c))))

(defun-inline cadr (c) (car (cdr c)))

(defun-inline cdaaar (c) (cdr (car (car (car c)))))

(defun-inline cdaadr (c) (cdr (car (car (cdr c)))))

(defun-inline cdaar (c) (cdr (car (car c))))

(defun-inline cdadar (c) (cdr (car (cdr (car c)))))

(defun-inline cdaddr (c) (cdr (car (cdr (cdr c)))))

(defun-inline cdadr (c) (cdr (car (cdr c))))

(defun-inline cdar (c) (cdr (car c)))

(defun-inline cddaar (c) (cdr (cdr (car (car c)))))

(defun-inline cddadr (c) (cdr (cdr (car (cdr c)))))

(defun-inline cddar (c) (cdr (cdr (car c))))

(defun-inline cdddar (c) (cdr (cdr (cdr (car c)))))

(defun-inline cddddr (c) (cdr (cdr (cdr (cdr c)))))

(defun-inline cdddr (c) (cdr (cdr (cdr c))))

(defun-inline cddr (c) (cdr (cdr c)))


(defun-inline first (list) (car list))

(defun-inline second (list) (cadr list))

(defun-inline third (list) (caddr list))

(defun-inline fourth (list) (cadddr list))

(defun-inline fifth (list) (nth 4 list))

(defun-inline sixth (list) (nth 5 list))

(defun-inline seventh (list) (nth 6 list))

(defun-inline eighth (list) (nth 7 list))

(defun-inline ninth (list) (nth 8 list))

(defun-inline tenth (list) (nth 9 list))


(defun accumulate-rl (op n args)
  (loop for i from (- n 2) downto 0
	as result = (funcall op (svref args (- n 2)) (svref args (- n 1)))
	then (funcall op (svref args i) result)
	finally (return result)))

(defun acons (key data a-list)
  (cons (cons key data) a-list))

(defun adjoin/4 (item list test key)
  (if (member/4 item list test key)
      list 
      (cons item list)))

(defun adjoin (item list &key test test-not (key #'identity))
  (adjoin/4 item
	    list
	    (if (null test)
		(if (null test-not)
		    #'eql
		    #'(lambda (x y) (not (funcall test-not x y))))
		test)
	    key))

;;; Y can be anything because of stupid CL history....
(defmethod append/2 ((x list) y)
  (if (null x)
      y
      (cons (car x) (append/2 (cdr x) y))))

(defun append (&restv args)
  (let ((n (length args)))
    (case n
      (0 nil)
      (1 (svref args 0))
      (t (accumulate-rl #'append/2 n args)))))

(defun assoc/4 (x a-list test key)
  (loop for entry in a-list
	when (funcall test (funcall key (car entry)) x)
	return entry))

(defun assoc (x a-list &key test test-not (key #'identity))
  (assoc/4 x
	   a-list
	   (if (null test)
	       (if (null test-not)
		   #'eql
		   #'(lambda (x y) (not (funcall test-not x y))))
	       test)
	   key))

(defun assq (x a-list)
  (loop for entry in a-list
	when (eq (car entry) x)
	return entry))

(defun assql (x a-list)
  (loop for entry in a-list
	when (eql (car entry) x)
	return entry))

(defun butlast/2 (list n)
  (let ((len (length list)))
    (if (<= n len)
	(loop for i from 0 below (- len n)
	      for x in list collect x)
	nil)))

(defun butlast (list &optional (n 1))
  (butlast/2 list n))

;;; SPICE!
(defun copy-alist (alist)
  (if (atom alist)
      (if alist
	  (error "~S is not a list." alist))
      (let ((result
	     (cons (if (atom (car alist))
		       (car alist)
		       (cons (caar alist) (cdar alist)) )
		   '() )))	      
	(do ((x (cdr alist) (cdr x))
	     (splice result
		     (cdr (rplacd splice
				  (cons
				   (if (atom (car x)) 
				       (car x)
				       (cons (caar x) (cdar x)))
				   '() ))) ))
	    ;; Non-null terminated alist done here.
	    ((atom x) (unless (null x)
			(rplacd splice x))
	     result)))))

(defun copy-list (list)
  (if (atom list)
      list
      (cons (car list) (copy-list (cdr list)))))

(defun copy-tree (tree)
  (if (atom tree)
      tree
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))

(defun-inline endp (x)
  (null x))


(defun intersection (list1 list2  &key (key #'identity)
			   (test #'eql testp) (test-not nil notp))
  (let ((res nil))
    (dolist (elt list1)
      (if (with-set-keys (member (funcall key elt) list2))
	  (push elt res)))
    res))

(defun last (l)
  (if (consp (cdr l))
      (last (cdr l))
      l))

;;; SPICE!
(defun ldiff (list sublist)
  (do* ((list list (cdr list))
	(result (list ()))
	(splice result))
       ((or (null list) (eq list sublist)) (cdr result))
    (setq splice (cdr (rplacd splice (list (car list)))))))

(defun length/list (x)
  (loop for e in x
	counting x into len
	finally (return len)))

;;; Straight out of Cltl2
(defun list-length (x)
  (do ((n 0 (+ n 2))			; counter
       (fast x (cddr fast))		; fast pointer leaps by 2
       (slow x (cdr slow)))		; slow pointer leaps by 1
      (nil)
    ;; If fast pointer hits the end, return the count
    (when (endp fast) (return n))
    (when (endp (cdr fast)) (return (+ n 1)))
    ;; If fast pointer eventually equals slow pointer,
    ;; then we must be stuck in a circular list.
    ;; (A deeper property is the converse: if we are
    ;; stuck in a circular list, then eventually the
    ;; fast pointer will equal the slow pointer.
    ;; That fact justifies this implementation.)
    (when (and (eq fast slow) (> n 0)) (return nil))))

(defun list*-1 (rest)
  (if (null (cdr rest))
      (car rest)
      (cons (car rest) (list*-1 (cdr rest)))))

(defun list* (&rest args)
  (declare (dynamic-extent args))
  (list*-1 args))

(defun list (&rest l) l)

(defun make-list-1 (size initial-element)
  (loop for i from 0 below size collect initial-element))

(defun make-list (size &key initial-element)
  (make-list-1 size initial-element))

(defun mapc (function list)
  (if (null list)
      list
      (progn (funcall function (car list))
	     (mapc function (cdr list)))))

(defun mapcan/2 (func arg-lists)
  (loop for a in arg-lists
	when (null a) return nil
	collect (car a) into args
	collect (cdr a) into rest
	finally (return (nconc (apply func args) (mapcan/2 func rest)))))

(defun mapcan (func &rest arg-lists)
  (declare (dynamic-extent arg-lists))
  (mapcan/2 func arg-lists))

(defun mapcar/2 (func arg-lists)
  (loop for a in arg-lists
	when (null a) return nil
	collect (car a) into args
	collect (cdr a) into rest
	finally (return (cons (apply func args) (mapcar/2 func rest)))))

(defun mapcar (func &rest arg-lists)
  (declare (dynamic-extent arg-lists))
  (mapcar/2 func arg-lists))


(defun member/4 (x list test  key)
  (cond ((atom list) nil)
	((funcall test (funcall key (car list)) x) list)
	(t (member/4 x (cdr list) test key))))

(defun member-if (test list &key (key #'identity))
  (loop for rest on list
	when (funcall test (funcall key (car rest)))
	return rest
	finally (return nil)))

(defun member (x list &key test test-not (key #'identity))
  (member/4 x
	    list
	    (if (null test)
		(if (null test-not)
		    #'eql
		    #'(lambda (x y) (not (funcall test-not x y))))
		test)
	    key))

(defun memq (e l)
  (cond ((atom l) nil)			; handle dotted lists
	((eq e (car l)) l)
	(t (memq e (cdr l)))))

(defun memql (e l)
  (cond ((atom l) nil)			; handle dotted lists
	((eql e (car l)) l)
	(t (memq e (cdr l)))))


;;; SPICE!
(defun nbutlast (list &optional (n 1))
  (declare (list list) (type integer n))
  (if (< n 0) (setq n 0))
  (let ((length (1- (length list))))
    (declare (fixnum length))
    (if (< length n) ()
	(do ((1st (cdr list) (cdr 1st))
	     (2nd list 1st)
	     (count length (1- count)))
	    ((= count n)
	     (rplacd 2nd ())
	     list)))))

(defun nconc/2 (x y)
  (if (null x)
      y
      (loop for c on x by #'cdr
	    when (atom (cdr c))
	    return (progn (setf (cdr c) y)
			  x))))

;;; Use common code?
(defun nconc (&restv args)
  (let ((n (length args)))
    (case n
      (0 nil)
      (1 (svref args 0))
      (t (accumulate-rl #'nconc/2 n args)))))

;;; SPICE!
(defun nintersection (list1 list2 &key (key #'identity)
			    (test #'eql testp) (test-not nil notp))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (do () ((endp list1))
      (if (with-set-keys (member (funcall key (car list1)) list2))
	  (steve-splice list1 res)
	  (setq list1 (Cdr list1))))
    res))

(defun nreconc (x y) (nconc (nreverse x) y))

(defun nreverse/list (l)
  (labels ((doit (head tail)
	     (let ((rest (cdr tail)))
	       (setf (cdr tail) head)
	       (if (null rest)
		   tail
		   (doit tail rest)))))
    (if (null l)
	l
	(doit nil l))))

;;; SPICE
(defun nset-difference (list1 list2 &key (key #'identity)
			      (test #'eql testp) (test-not nil notp))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (do () ((endp list1))
      (if (not (with-set-keys (member (funcall key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))

;;; From SPICE 
;;; The outer loop examines list1 while the inner loop examines list2. If an
;;; element is found in list2 "equal" to the element in list1, both are
;;; spliced out. When the end of list1 is reached, what is left of list2 is
;;; tacked onto what is left of list1.  The splicing operation ensures that
;;; the correct operation is performed depending on whether splice is at the
;;; top of the list or not

(defun nset-exclusive-or (list1 list2 &key (test #'eql) (test-not nil notp)
				(key #'identity))
  (do ((x list1 (cdr x))
       (splicex ()))
      ((endp x)
       (if (null splicex)
	   (setq list1 list2)
	   (rplacd splicex list2))
       list1)
    (do ((y list2 (cdr y))
	 (splicey ()))
	((endp y) (setq splicex x))
      (cond ((if notp
		 (not (funcall test-not (funcall key (car x))
			       (funcall key (Car y))))
		 (funcall test (funcall key (car x)) (funcall key (Car y))))
	     (if (null splicex)
		 (setq list1 (cdr x))
		 (rplacd splicex (cdr x)))
	     (if (null splicey) 
		 (setq list2 (cdr y))
		 (rplacd splicey (cdr y)))
	     (return ()))			; assume lists are really sets
	    (t (setq splicey y))))))

(defun nsubst (new old tree &key (key #'identity)
		  (test #'eql testp) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (satisfies-the-test old subtree)
				 (setf (cdr last) new)))
			 (if (satisfies-the-test old subtree)
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nth (i l)
  (if (= i 0)
      (car l)
      (nth (- i 1) (cdr l))))

(defun nthcdr (i l)
  (if (= i 0)
      l
      (nthcdr (- i 1) (cdr l))))

;;; From SPICE
(defun nunion (list1 list2 &key (key #'identity)
		     (test #'eql testp) (test-not nil notp))
  "Destructively returns the union list1 and list2."
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res list2))
    (do ()
	((endp list1))
      (if (not (with-set-keys (member (funcall key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setf list1 (cdr list1))))
    res))
  
(defun pairlis/3 (keys values a-list)
  (if (or (null  keys) (null values))
      a-list
      (pairlis/3 (cdr keys)
		 (cdr values)
		 (cons (cons (car keys) (car values))
		       a-list))))

(defun-inline pairlis (keys values &optional a-list)
  (pairlis/3 keys values a-list))

(defun rassoc/4 (x a-list test key)
  (loop for entry in a-list
	when (funcall test (funcall key (cdr entry)) x)
	return entry))

(defun rassoc (x a-list &key test test-not (key #'identity))
  (rassoc/4 x
	    a-list
	    (if (null test)
		(if (null test-not)
		    #'eql
		    #'(lambda (x y) (not (funcall test-not x y))))
		test)
	    key))


(defun-inline rest (list) (cdr list))

;;; SPICE!
(defun revappend (x y)
  (do ((top x (cdr top))
       (result y (cons (car top) result)))
      ((endp top) result)))


(defun reverse/list (s)
  (labels ((doit (new rest)
	     (if (null rest)
		 new
		 (doit (cons (car rest) new) (cdr rest)))))
    (doit nil s)))

;;; SPICE
(defun set-difference (list1 list2 &key (key #'identity)
			     (test #'eql testp) (test-not nil notp))
  "Returns the elements of list1 which are not in list2."
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (if (null list2)
      list1
      (let ((res nil))
	(dolist (elt list1)
	  (if (not (with-set-keys (member (funcall key elt) list2)))
	      (push elt res)))
	res)))

;;; SPICE
(defun set-exclusive-or (list1 list2 &key (key #'identity)
			       (test #'eql testp) (test-not nil notp))
  (let ((result nil))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall key elt) list2))
	(setq result (cons elt result))))
    (dolist (elt list2)
      (unless (with-set-keys (member (funcall key elt) list1))
	(setq result (cons elt result))))
    result))


(defun set-nth (i l value)
  (declare (fixnum i) (list l))
  (if (= i 0)
      (setf (car l) value)
      (set-nth (1- i) (cdr l) value)))

(defun sublis/4 (alist tree test key)
  (let ((subst (assoc/4 tree alist test key)))
    (if (null subst)
	(if (atom tree)
	    tree
	    (cons (sublis/4 alist (car tree) test key)
		  (sublis/4 alist (cdr tree) test key)))
	(cdr subst))))

(defun sublis (alist tree &key test test-not (key #'identity))
  (sublis/4 alist
	    tree
	    (if (null test)
		(if (null test-not)
		    #'eql
		    #'(lambda (x y) (not (funcall test-not x y))))
		test)
	    key))

;;; SPICE
(defun subsetp (list1 list2 &key (key #'identity)
		      (test #'eql testp) (test-not nil notp))
  (dolist (elt list1)
    (unless (with-set-keys (member (funcall key elt) list2))
      (return-from subsetp nil)))
  t)

(defun subst (new old tree)
  (if (atom tree)
      (if (eql tree old) new tree)
      (cons (subst new old (car tree))
	    (subst new old (cdr tree)))))

;;; From SPICE
;;; This function assumes list2 is the result, adding to it from list1 as
;;; necessary.  List2 must initialize the result value, so the call to MEMBER
;;; will apply the test to the elements from list1 and list2 in the correct
;;; order.
(defun union (list1 list2 &key
		    (key #'identity) (test #'eql testp) (test-not nil notp))
  "Returns the union of list1 and list2."
  (when (and testp notp) (error "Test and test-not both supplied."))
  (let ((res list2))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall key elt) list2))
	(push elt res)))
    res))
