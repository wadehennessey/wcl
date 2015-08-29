;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun-inline make-string (n &key (initial-element #\Null))
  (make-simple-string n initial-element))

(defun nstring-capitalize (string &key start end)
  (error "write me"))

(defun nstring-upcase (string &key start end)
  (error "write me"))

(defmethod-inline simple-string= ((s1 simple-string) (s2 simple-string))
  (%= (strcmp s1 s2) 0))

(defun string-capitalize (string &key start end)
  (error "write me"))

(defun string-compare (s1 s2 compare)
  (let ((l1 (length s1))
	(l2 (length s2)))
    (if (= l1 l2)
	(loop for i from 0 below (max l1 l2)
	      when (= i l1)  return t
	      when (= i l2) return nil
	      unless (funcall compare (aref s1 i) (aref s2 i)) return nil
	      finally (return t))
	nil)))

(defun-inline string-downcase (s &key (start 0) end)
  (string-downcase/3 s start end))

(defun string-downcase/3 (s start end)
  (let* ((string (string s))
	 (last (or end (length string))))
    (nstring-downcase/4 (make-string (- last start)) string start last)))

(defun-inline nstring-downcase (string &key (start 0) end)
  (nstring-downcase/4 string string start (or end (length string))))

(defun nstring-downcase/4 (new old start end)
  (loop for i from start below end
	do (setf (aref new i) (char-downcase (aref old i))))
  new)

(defun string= (s1 s2)
  (let ((x1 (string s1))
	(x2 (string s2)))
    (if (and (simple-string-p s1) (simple-string-p s2))
	(simple-string= s1 s2)
	(let ((l1 (length x1))
	      (l2 (length x2)))
	  (and (= l1 l2)
	       (dotimes (i l1 t)
		 (unless (char= (aref x1 i) (aref x2 i))
		   (return nil))))))))

(defun string-equal (s1 s2 &key (start-1 0) limit-1 (start-2 0) limit-2)
  (let* ((string-1 (string s1))
	 (string-2 (string s2))
	 (end-1 (or limit-1 (length string-1)))
	 (end-2 (or limit-2 (length string-2))))
    (if (= (- end-1 start-1) (- end-2 start-2))
	(loop for i1 from start-1 below end-1
	      for i2 from start-2 below end-2
	      unless (char-equal (aref string-1 i1) (aref string-2 i2))
	      do (return nil)
	      finally (return t))
	nil)))

(defun string-left-trim (char-bag string)
  (subseq string (trim-left-index char-bag string)))

(defun string-lessp (s1 s2 &key start1 end1 start2 end2)
  (string-compare s1 s2 #'char-lessp/2))

(defun string-right-trim (char-bag string)
  (subseq string 0 (trim-right-index char-bag string)))

(defun string-trim (char-bag string)
  (subseq string
	  (trim-left-index char-bag string)
	  (trim-right-index char-bag string)))

(defun string-upcase (s)
  (let* ((string (string s))
	 (len (length string))
	 (new (make-string len)))
    (dotimes (i len new)
      (setf (schar new i) (char-upcase (aref string i))))))

(defun string (x)
  (etypecase x
    (string x)
    (symbol (symbol-name x))
    (character (let ((s (make-string 1)))
		 (setf (schar s 0) x)
		 s))))

(defun trim-left-index (char-bag string)
  (let ((len (length string)))
    (dotimes (i len len)
      (unless (find (aref string i) char-bag)
	(return i)))))

(defun trim-right-index (char-bag string)
  (loop for i from (length string) above 0
	unless (find (aref string (- i 1)) char-bag)
	do (return i)
	finally (return 0)))


