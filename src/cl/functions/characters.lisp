;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun alpha-char-p (char)
  (or (and (char>= char #\A) (char<= char #\Z))
      (and (char>= char #\a) (char<= char #\z))))

(defun alphanumericp (char)
  (or (alpha-char-p char) (digit-char-p/2  char 10)))

(defmethod-inline char-bits ((c character))
  0)

(defmethod-inline char-code ((c character))
  (%char-code c))

(defun char-downcase (char)
  (if (and (char>= char #\A) (char<= char #\Z))
      (code-char (+ (char-code #\a) (- (char-code char) (char-code #\A))))
      char))

(defmethod-inline char=/2 ((char1 character) (char2 character))
  (%eq char1 char2))

(def-nary-pred char= char=/2)

(defun char-equal/2 (char1 char2)
  (char= (char-downcase char1) (char-downcase char2)))

(def-nary-pred char-equal  char-equal/2)

(defmethod-inline char>=/2 ((char1 character) (char2 character))
  (%>= (char-code char1) (char-code char2)))

(def-nary-pred char>= char>=/2)

(defun char-greaterp/2 (char1 char2)
  (char> (char-downcase char1) (char-downcase char2)))

(def-nary-pred char-greaterp char-greaterp/2)

(defun char>/2 (char1 char2)
  (%> (char-code char1) (char-code char2)))

(def-nary-pred char> char>/2)

(def-nary-pred char< char</2)

(defun-inline char<=/2 (char1 char2)
  (%<= (char-code char1) (char-code char2)))

(def-nary-pred char<= char<=/2)

(defun char-lessp/2 (char1 char2)
  (char< (char-downcase char1) (char-downcase char2)))

(def-nary-pred char-lessp char-lessp/2)

(defun-inline char</2 (char1 char2)
  (< (char-code char1) (char-code char2)))

(defmethod char-name ((char character))
  (let ((code (char-code char)))
    (if (> code 127)
	(format nil "c~X" code)
	(car (rassoc char *character-names* :test #'char=)))))

(def-nary-pred char/= char/=/2)

(defun char-not-equal/2 (char1 char2)
  (char/= (char-downcase char1) (char-downcase char2)))

(def-nary-pred char-not-equal char-not-equal/2)

(defun char-not-greaterp/2 (char1 char2)
  (char>= (char-downcase char1) (char-downcase char2)))

(def-nary-pred char-not-greaterp char-not-greaterp/2)

(defun char-not-lessp/2 (char1 char2)
  (char<= (char-downcase char1) (char-downcase char2)))

(def-nary-pred char-not-lessp char-not-lessp/2)

(defun char/=/2 (char1 char2)
  (not (char=/2 char1 char2)))

(defun char-upcase (char)
  (if (and (char>= char #\a) (char<= char #\z))
      (code-char (%+ (char-code #\A) (%- (char-code char) (char-code #\a))))
      char))

(defmethod-inline code-char ((n fixnum))
  (if (%>= n char-code-limit)
      nil
      (%code-char n)))

(defun digit-char-p/2 (char radix)
  (declare (fixnum radix))
  (let ((weight (%- (char-code char) (char-code #\0))))
    (declare (fixnum weight))
    (cond ((<= radix 10) (if (and (>= weight 0) (< weight radix))
			     weight
			     nil))
	  ((> radix 36) (error "radix ~D is larger than 36" radix))
	  ((and (>= weight 0) (< weight 10)) weight)
	  ;; YUK! Change this spice stuff....
	  ((and (>= (setq weight (%- weight 7)) 10) ; upper case A - Z.
		(< weight radix))
	   weight)
	  ((and (>= (setq weight (%- weight 32)) 10) ; lower case a - z.
		(< weight radix))
	   weight)
	  (t nil))))


(defun-inline digit-char-p (char &optional (radix 10))
  (digit-char-p/2  char radix))

;;; SPICE!
(defun lower-case-p (char)
  (< 96
     (char-code char)
     123))

(defun both-case-p (char)
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123))))

(defun graphic-char-p (char)
  (< 31
     (char-code char)
     127))

(defun name-char (name)
  (cdr (assoc (string name) *character-names* :test  #'string-equal)))

(defmethod-inline set-char-bit ((c character) (bit-name symbol) (value fixnum))
  value)

(defun-inline standard-char-p (char)
  (characterp char))

(defun upper-case-p (char)
  (and (char>= char #\A) (char<= char #\Z)))



