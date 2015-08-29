;;; Copyright 1989,1990 by the Massachusetts Institute of Technology,
;;; Cambridge, Massachusetts.

(in-package "XP")

(defvar *xp-printing-functions*
  '(write print prin1 princ pprint format write-to-string princ-to-string
    prin1-to-string write-line write-string write-char terpri fresh-line
    defstruct finish-output force-output clear-output)
  "printing functions redefined by xp.")

;;; Must do the following in common lisps not supporting *print-shared*

(defvar *print-shared* nil)

(defvar *print-pprint-dispatch* t ;see initialization at end of file.
  "controls pretty printing of output")

(defvar *print-right-margin* nil
  "+#/nil the right margin for pretty printing")

(defvar *print-miser-width* 40.
  "+#/nil miser format starts when there is less than this width left")

(defvar *print-lines* nil
  "+#/nil truncates printing after # lines")

(defvar *default-right-margin* 70.
  "controls default line length; must be a non-negative integer")

(defvar *last-abbreviated-printing*
  #'(lambda (&optional stream) (declare (ignore stream)) nil)
  "funcalling this redoes the last xp printing that was abbreviated.")

(defvar *ipd* nil ;see initialization at end of file.
  "initial print dispatch table.")

(defvar *current-level* 0
  "current depth in logical blocks.")

(defvar *current-length* 0
  "current position in logical block.")

(defvar *abbreviation-happened* nil
  "t if current thing being printed has been abbreviated.")

(defvar *result* nil "used to pass back a value")

(defvar *locating-circularities* nil
  "Integer if making a first pass over things to identify circularities.
   Integer used as counter for #n= syntax.")
(defvar *parents* nil "used when *print-shared* is nil")

(defvar *circularity-hash-table* nil
  "Contains hash table used for locating circularities, or a stack.")

;;; When an entry is first made it is zero.
;;; If a duplicate is found, a positive integer tag is assigned.
;;; After the first time the object is printed out, the tag is negated.

(defvar *free-circularity-hash-tables* nil
  "free list of circularity hash tables") ; never bound

(defvar *preds-for-specs*
  '((T always-true) (cons consp) (simple-atom simple-atom-p) (other otherp)
    (null null) (symbol symbolp) (atom atom) (cons consp)
    (list listp) (number numberp) (integer integerp)
    (rational rationalp) (float floatp) (complex complexp)
    (character characterp) (string stringp) (bit-vector bit-vector-p)
    (vector vectorp) (simple-vector simple-vector-p) 
    (simple-string simple-string-p) (simple-bit-vector simple-bit-vector-p)
    (array arrayp) (package packagep) (function functionp)
    (compiled-function compiled-function-p) (common commonp)))

(defvar *describe-xp-streams-fully* nil "Set to T to see more info.")

(defvar *free-xps* nil "free list of XP stream objects") ; never bound

(defvar *format-string-cache* T)

(defvar *fn-table* (make-hash-table) "used to access fns for commands")

(proclaim '(special *string* *used-args* *used-outer-args* *used-initial*
	    *get-arg-carefully* *inner-end* *outer-end* *at-top*
	    *default-package*))

(defvar *testing-errors* nil "Used only when testing XP")

(lisp:defstruct (pprint-dispatch (:conc-name nil) (:copier nil))
  (conses-with-cars (make-hash-table :test #'eq) :type hash-table)
  (structures (make-hash-table :test #'eq) :type hash-table)
  (others nil :type list))

;;; The list and the hash-tables contain entries of the
;;; following form.  When stored in the hash tables, the test entry is 
;;; the number of entries in the OTHERS list that have a higher priority.
(lisp:defstruct (entry (:conc-name nil))
  (test nil)				; predicate function or
					; count of higher priority others.
  (fn nil)				; pprint function
  (full-spec nil))			; list of priority and type specifier

(eval-when (eval load compile)
  (defun structure-type-p (x)
    (gethash x lisp::*structure-info*))
  
  (defun output-width (&optional (s *standard-output*))
    (declare (ignore s))
    nil)
  
  (defun output-position (&optional (s *standard-output*))
    (lisp::stream-column s))
  )

(defun install (&key (package *package*) (macro nil) (shadow T) (remove nil))
  (when (not (packagep package)) (setq package (find-package package)))
  (when (not remove)
    (when macro
      (set-dispatch-macro-character #\# #\" #'format-string-reader))
    (when (not (eq package (find-package "XP")))
      (use-package "XP" package)
      (when shadow (shadowing-import *xp-printing-functions* package))))
  (when (and remove (member (find-package "XP") (package-use-list package)))
    (unuse-package "XP" package)
    (dolist (sym (intersection *xp-printing-functions*
			       (package-shadowing-symbols package)))
      (unintern sym package)))
  T)

(defun get-circularity-hash-table ()
  (let ((table (pop *free-circularity-hash-tables*)))
    (if table table (make-hash-table :test 'eq))))

;;; If you call this, then the table gets efficiently recycled.
(defun free-circularity-hash-table (table)
  (clrhash table)
  (pushnew table *free-circularity-hash-tables*))

;;;                       ---- DISPATCHING ----

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (when (null table) (setq table *IPD*))
  (let* ((new-conses-with-cars
	  (make-hash-table :test #'eq
			   :size (max (hash-table-count
				       (conses-with-cars table)) 32)))
	 (new-structures
	  (make-hash-table :test #'eq
			   :size (max (hash-table-count (structures table))
				      32))))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-conses-with-cars) (copy-entry value)))
	     (conses-with-cars table))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-structures) (copy-entry value)))
	     (structures table))
    (make-pprint-dispatch
     :conses-with-cars new-conses-with-cars
     :structures new-structures
     :others (copy-list (others table)))))

(defun set-pprint-dispatch (type-specifier function
					   &optional
					   (priority 0)
					   (table *print-pprint-dispatch*))
  (when (or (not (numberp priority)) (complexp priority))
    (error "invalid PRIORITY argument ~A to SET-PPRINT-DISPATCH" priority))
  (set-pprint-dispatch+ type-specifier function priority table))

(defun set-pprint-dispatch+ (type-specifier function priority table)
  (let* ((category (specifier-category type-specifier))
	 (pred
	  (if (not (eq category 'other))
	      nil
	      (let ((pred (specifier-fn type-specifier)))
		(if (and (consp (caddr pred))
			 (symbolp (caaddr pred))
			 (equal (cdaddr pred) '(x)))
		    (symbol-function (caaddr pred))
		    (compile nil pred)))))
	 (entry (if function
		    (make-entry :test pred
				:fn function
				:full-spec (list priority type-specifier)))))
    (case category
      (cons-with-car
       (cond ((null entry)
	      (remhash (cadadr type-specifier) (conses-with-cars table)))
	     (T (setf (test entry)
		      (count-if #'(lambda (e)
				    (priority-> (car (full-spec e)) priority))
				(others table)))
		(setf (gethash (cadadr type-specifier)
			       (conses-with-cars table)) entry))))
      (structure-type
       (cond ((null entry) (remhash type-specifier (structures table)))
	     (T (setf (test entry)
		      (count-if #'(lambda (e)
				    (priority-> (car (full-spec e)) priority))
				(others table)))
		(setf (gethash type-specifier (structures table)) entry))))
      (T				;other
       (let ((old (car (member type-specifier (others table) :test #'equal
			       :key #'(lambda (e) (cadr (full-spec e)))))))
	 (when old
	   (setf (others table) (delete old (others table)))
	   (adjust-counts table (car (full-spec old)) -1)))
       (when entry
	 (let ((others (cons nil (others table))))
	   (do ((l others (cdr l)))
	       ((null (cdr l)) (rplacd l (list entry)))
	     (when (priority-> priority (car (full-spec (cadr l))))
	       (rplacd l (cons entry (cdr l)))
	       (return nil)))
	   (setf (others table) (cdr others)))
	 (adjust-counts table priority 1)))))
  nil)

(defun priority-> (x y)
  (if (consp x)
      (if (consp y) (> (car x) (car y)) nil)
      (if (consp y) T (> x y))))

(defun adjust-counts (table priority delta)
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (if (priority-> priority (car (full-spec value)))
		   (incf (test value) delta)))
	   (conses-with-cars table))
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (if (priority-> priority (car (full-spec value)))
		   (incf (test value) delta)))
	   (structures table)))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (when (null table) (setq table *IPD*))  
  (let ((fn (get-printer object table)))
    (values (or fn #'non-pretty-print) (not (null fn)))))

(defun get-printer (object table)
  (let* ((entry (if (consp object)
		    (gethash (car object) (conses-with-cars table))
		    (gethash (type-of object) (structures table)))))
    (if (not entry)
	(setq entry (find object (others table) :test #'fits))
	(do ((i (test entry) (1- i))
	     (l (others table) (cdr l)))
	    ((zerop i))
	  (when (fits object (car l)) (setq entry (car l)) (return nil))))
    (when entry (fn entry))))

(defun fits (obj entry) (funcall (test entry) obj))

(defun specifier-category (spec)
  (cond ((and (consp spec)
	      (eq (car spec) 'cons)
	      (consp (cdr spec))
	      (null (cddr spec))
	      (consp (cadr spec))
	      (eq (caadr spec) 'member)
	      (consp (cdadr spec))
	      (null (cddadr spec)))
	 'cons-with-car)
	((and (symbolp spec) (structure-type-p spec)) 'structure-type)
	(T 'other)))

(defun always-true (x) (declare (ignore x)) T)

(defun specifier-fn (spec)
  `(lambda (x) ,(convert-body spec)))

(defun convert-body (spec)
  (cond ((atom spec)
	 (let ((pred (cadr (assoc spec *preds-for-specs*))))
	   (if pred `(,pred x) `(typep x ',spec))))
	((member (car spec) '(and or not))
	 (cons (car spec) (mapcar #'convert-body (cdr spec))))
	((eq (car spec) 'member)
	 `(member x ',(copy-list (cdr spec))))
	((eq (car spec) 'cons)
	 `(and (consp x)
	   ,@(if (cdr spec) `((let ((x (car x)))
				,(convert-body (cadr spec)))))
	   ,@(if (cddr spec) `((let ((x (cdr x)))
				 ,(convert-body (caddr spec)))))))
	((eq (car spec) 'satisfies)
	 `(funcall (function ,(cadr spec)) x))
	(T `(typep x ',(copy-tree spec)))))

;;;                ---- XP STRUCTURES, AND THE INTERNAL ALGORITHM ----

(eval-when (eval load compile) ;not used at run time.
  (defvar block-stack-entry-size 1)
  (defvar prefix-stack-entry-size 5)
  (defvar queue-entry-size 7)
  (defvar buffer-entry-size 1)
  (defvar prefix-entry-size 1)
  (defvar suffix-entry-size 1))

(eval-when (eval load compile) ;used at run time
  (defvar block-stack-min-size #.(* 35. block-stack-entry-size))
  (defvar prefix-stack-min-size #.(* 30. prefix-stack-entry-size))
  (defvar queue-min-size #.(* 75. queue-entry-size))
  (defvar buffer-min-size 256.)
  (defvar prefix-min-size 256.)
  (defvar suffix-min-size 256.)) 

(lisp:defstruct (xp-structure (:conc-name nil) (:print-function describe-xp))
  (base-stream nil)			; The stream io eventually goes to.
  linel					; The line length to use in formatting.
  line-limit				; If true, max  lines to print.
  line-no				; number of next line to be printed.
  char-mode				; NIL :UP :DOWN :CAP0 :CAP1 :CAPW
  char-mode-counter			; depth of nesting of ~(...~)
  depth-in-blocks
  ;; Number of logical blocks at QRIGHT that are started but not ended.
  (block-stack (make-array #.block-stack-min-size))
  BLOCK-STACK-PTR
  ;; This stack is pushed and popped in accordance with the way blocks are 
  ;; nested at the moment they are entered into the queue.  It contains the 
  ;; following block specific value.
  ;; SECTION-START total position where the section (see AIM-1102)
  ;; that is rightmost in the queue started.
  (BUFFER (make-array #.buffer-min-size
		      :element-type 'string-char))
  charpos
  buffer-ptr
  buffer-offset
  ;; This is a vector of characters (eg a string) that builds up the
  ;; line images that will be printed out.  BUFFER-PTR is the
  ;; buffer position where the next character should be inserted in
  ;; the string.  CHARPOS is the output character position of the
  ;; first character in the buffer (non-zero only if a partial line
  ;; has been output).  BUFFER-OFFSET is used in computing total lengths.
  ;; It is changed to reflect all shifting and insertion of prefixes so that
  ;; total length computes things as they would be if they were 
  ;; all on one line.  Positions are kept three different ways
  ;; Buffer position (eg BUFFER-PTR)
  ;; Line position (eg (+ BUFFER-PTR CHARPOS)).  Indentations are stored in this form.
  ;; Total position if all on one line (eg (+ BUFFER-PTR BUFFER-OFFSET))
  ;;  Positions are stored in this form.
  (QUEUE (make-array #.queue-min-size)) QLEFT QRIGHT
  ;; This holds a queue of action descriptors.  QLEFT and QRIGHT
  ;; point to the next entry to dequeue and the last entry enqueued
  ;; respectively.  The queue is empty when
  ;; (> QLEFT QRIGHT).  The queue entries have several parts:
  ;; QTYPE one of :NEWLINE/:IND/:START-BLOCK/:END-BLOCK
  ;; QKIND :LINEAR/:MISER/:FILL/:MANDATORY or :UNCONDITIONAL/:FRESH
  ;;  or :BLOCK/:CURRENT
  ;; QPOS total position corresponding to this entry
  ;; QDEPTH depth in blocks of this entry.
  ;; QEND offset to entry marking end of section this entry starts. (NIL until known.)
  ;;  Only :start-block and non-literal :newline entries can start sections.
  ;; QOFFSET offset to :END-BLOCK for :START-BLOCK (NIL until known).
  ;; QARG for :IND indentation delta
  ;;     for :START-BLOCK suffix in the block if any.
  ;;                      or if per-line-prefix then cons of suffix and
  ;;                      per-line-prefix.
  ;;     for :END-BLOCK suffix for the block if any.
  (PREFIX (make-array #.buffer-min-size :element-type
		      #-symbolics 'string-char #+symbolics 'character))
  ;; this stores the prefix that should be used at the start of the line
  (PREFIX-STACK (make-array #.prefix-stack-min-size)) PREFIX-STACK-PTR
  ;; This stack is pushed and popped in accordance with the way blocks 
  ;; are nested at the moment things are taken off the queue and printed.
  ;; It contains the following block specific values.
  ;; PREFIX-PTR current length of PREFIX.
  ;; SUFFIX-PTR current length of pending suffix
  ;; NON-BLANK-PREFIX-PTR current length of non-blank prefix.
  ;; INITIAL-PREFIX-PTR prefix-ptr at the start of this block.
  ;;SECTION-START-LINE line-no value at last non-literal break at this level.
  (SUFFIX (make-array #.buffer-min-size :element-type 'string-char)))

;;; this stores the suffixes that have to be printed to close of the current
;;; open blocks.  For convenient in popping, the whole suffix
;;; is stored in reverse order.


(defmacro LP<-BP (xp &optional (ptr nil))
  (if (null ptr) (setq ptr `(buffer-ptr ,xp)))
  `(+ ,ptr (charpos ,xp)))

(defmacro TP<-BP (xp)
  `(+ (buffer-ptr ,xp) (buffer-offset ,xp)))

(defmacro BP<-LP (xp ptr)
  `(- ,ptr (charpos ,xp)))

(defmacro BP<-TP (xp ptr)
  `(- ,ptr (buffer-offset ,xp)))

;;; This does not tell you the line position you were at when the TP
;;; was set, unless there have been no newlines or indentation output 
;;; between ptr and the current output point.
(defmacro LP<-TP (xp ptr)
  `(LP<-BP ,xp (BP<-TP ,xp ,ptr)))

;;; We don't use adjustable vectors or any of that, because we seldom have
;;; to actually extend and non-adjustable vectors are a lot faster in
;;; many Common Lisps.
(defmacro check-size (xp vect ptr)
  (let* ((min-size
	  (symbol-value
	   (intern (concatenate 'string (string vect) "-MIN-SIZE")
		   (find-package "XP"))))
	 (entry-size
	  (symbol-value
	   (intern (concatenate 'string (string vect) "-ENTRY-SIZE")
		   (find-package "XP")))))
    `(when (and (> ,ptr ,(- min-size entry-size)) ;seldom happens
	    (> ,ptr (- (length (,vect ,xp)) ,entry-size)))
      (let* ((old (,vect ,xp))
	     (new (make-array (+ ,ptr ,(if (= entry-size 1) 50
					   (* 10 entry-size)))
			      :element-type (array-element-type old))))
	(replace new old)
	(setf (,vect ,xp) new)))))

(defmacro section-start (xp) `(aref (block-stack ,xp) (block-stack-ptr ,xp)))

(defun push-block-stack (xp)
  (incf (block-stack-ptr xp) #.block-stack-entry-size)
  (check-size xp block-stack (block-stack-ptr xp)))

(defun pop-block-stack (xp)
  (decf (block-stack-ptr xp) #.block-stack-entry-size))

(defmacro prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (prefix-stack-ptr ,xp)))

(defmacro suffix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 1)))

(defmacro non-blank-prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 2)))

(defmacro initial-prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 3)))

(defmacro section-start-line (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 4)))

(defun push-prefix-stack (xp)
  (let ((old-prefix 0) (old-suffix 0) (old-non-blank 0))
    (when (not (minusp (prefix-stack-ptr xp)))
      (setq old-prefix (prefix-ptr xp)
	    old-suffix (suffix-ptr xp)
	    old-non-blank (non-blank-prefix-ptr xp)))
    (incf (prefix-stack-ptr xp) #.prefix-stack-entry-size)
    (check-size xp prefix-stack (prefix-stack-ptr xp))
    (setf (prefix-ptr xp) old-prefix)
    (setf (suffix-ptr xp) old-suffix)
    (setf (non-blank-prefix-ptr xp) old-non-blank)))

(defun pop-prefix-stack (xp)
  (decf (prefix-stack-ptr xp) #.prefix-stack-entry-size))

(defmacro Qtype   (xp index) `(aref (queue ,xp) ,index))

(defmacro Qkind   (xp index) `(aref (queue ,xp) (1+ ,index)))

(defmacro Qpos    (xp index) `(aref (queue ,xp) (+ ,index 2)))

(defmacro Qdepth  (xp index) `(aref (queue ,xp) (+ ,index 3)))

(defmacro Qend    (xp index) `(aref (queue ,xp) (+ ,index 4)))

(defmacro Qoffset (xp index) `(aref (queue ,xp) (+ ,index 5)))

(defmacro Qarg    (xp index) `(aref (queue ,xp) (+ ,index 6)))

;;; we shift the queue over rather than using a circular queue because
;;; that works out to be a lot faster in practice.  Note, short printout
;;; does not ever cause a shift, and even in long printout, the queue is
;;; shifted left for free every time it happens to empty out.
(defun enqueue (xp type kind &optional arg)
  (incf (Qright xp) #.queue-entry-size)
  (when (> (Qright xp) #.(- queue-min-size queue-entry-size))
    (replace (queue xp) (queue xp) :start2 (Qleft xp) :end2 (Qright xp))
    (setf (Qright xp) (- (Qright xp) (Qleft xp)))
    (setf (Qleft xp) 0))
  (check-size xp queue (Qright xp))
  (setf (Qtype xp (Qright xp)) type)
  (setf (Qkind xp (Qright xp)) kind)
  (setf (Qpos xp (Qright xp)) (TP<-BP xp))
  (setf (Qdepth xp (Qright xp)) (depth-in-blocks xp))
  (setf (Qend xp (Qright xp)) nil)
  (setf (Qoffset xp (Qright xp)) nil)
  (setf (Qarg xp (Qright xp)) arg))

(defmacro Qnext (index) `(+ ,index #.queue-entry-size))

(defun describe-xp (xp s depth) 
  (declare (ignore depth))
  (lisp:format s "#<XP stream ")
  (if (not (base-stream xp))
      (lisp:format s "not currently in use")
      (lisp:format s "outputting to ~S" (base-stream xp)))
  (when (base-stream xp)
    (lisp:format s "~&buffer= ~S" (subseq (buffer xp) 0 (max (buffer-ptr xp) 0)))
    (when (not *describe-xp-streams-fully*) (lisp:princ " ..." s))
    (when *describe-xp-streams-fully*
      (lisp:format s "~&   pos   _123456789_123456789_123456789_123456789")
      (lisp:format s "~&depth-in-blocks= ~D linel= ~D line-no= ~D line-limit= ~D"
		   (depth-in-blocks xp) (linel xp) (line-no xp) (line-limit xp))
      (when (or (char-mode xp) (not (zerop (char-mode-counter xp))))
	(lisp:format s "~&char-mode= ~S char-mode-counter= ~D"
		     (char-mode xp) (char-mode-counter xp)))
      (unless (minusp (block-stack-ptr xp))
	(lisp:format s "~&section-start")
	(do ((save (block-stack-ptr xp)))
	    ((minusp (block-stack-ptr xp)) (setf (block-stack-ptr xp) save))
	  (lisp:format s " ~D" (section-start xp))
	  (pop-block-stack xp)))
      (lisp:format s "~&linel= ~D charpos= ~D buffer-ptr= ~D buffer-offset= ~D"
		   (linel xp) (charpos xp) (buffer-ptr xp) (buffer-offset xp))
      (unless (minusp (prefix-stack-ptr xp))
	(lisp:format s "~&prefix= ~S"
		     (subseq (prefix xp) 0 (max (prefix-ptr xp) 0)))
	(lisp:format s "~&suffix= ~S"
		     (subseq (suffix xp) 0 (max (suffix-ptr xp) 0))))
      (unless (> (Qleft xp) (Qright xp))
	(lisp:format s "~&ptr type         kind           pos depth end offset arg")
	(do ((p (Qleft xp) (Qnext p))) ((> p (Qright xp)))
	  (lisp:format s "~&~4A~13A~15A~4A~6A~4A~7A~A"
		       (/ (- p (Qleft xp)) #.queue-entry-size)
		       (Qtype xp p)
		       (if (member (Qtype xp p) '(:newline :ind)) (Qkind xp p) "")
		       (BP<-TP xp (Qpos xp p))
		       (Qdepth xp p)
		       (if (not (member (Qtype xp p) '(:newline :start-block))) ""
			   (and (Qend xp p)
				(/ (- (+ p (Qend xp p)) (Qleft xp)) #.queue-entry-size)))
		       (if (not (eq (Qtype xp p) :start-block)) ""
			   (and (Qoffset xp p)
				(/ (- (+ p (Qoffset xp p)) (Qleft xp)) #.queue-entry-size)))
		       (if (not (member (Qtype xp p) '(:ind :start-block :end-block))) ""
			   (Qarg xp p)))))
      (unless (minusp (prefix-stack-ptr xp))
	(lisp:format s "~&initial-prefix-ptr prefix-ptr suffix-ptr non-blank start-line")
	(do ((save (prefix-stack-ptr xp)))
	    ((minusp (prefix-stack-ptr xp)) (setf (prefix-stack-ptr xp) save))
	  (lisp:format s "~& ~19A~11A~11A~10A~A"
		       (initial-prefix-ptr xp) (prefix-ptr xp) (suffix-ptr xp)
		       (non-blank-prefix-ptr xp) (section-start-line xp))
	  (pop-prefix-stack xp)))))
  (lisp:princ ">" s)
  (values))

;;; This maintains a list of XP structures.  We save them
;;; so that we don't have to create new ones all of the time.
;;; We have separate objects so that many can be in use at once.
;;; (Note should really be doing some locking here, but CL does not have the
;;; primitives for it.  There is a tiny probability here that two different
;;; processes could end up trying to use the same xp-stream)
(defun get-pretty-print-stream (stream)
  (let ((xp (pop *free-xps*)))
    (initialize-xp (if xp xp (make-xp-structure)) stream)))

;;; If you call this, the xp-stream gets efficiently recycled.
(defun free-pretty-print-stream (xp)
  (setf (base-stream xp) nil)
  (pushnew xp *free-xps*))

;;; This is called to initialize things when you start pretty printing.
(defun initialize-xp (xp stream)
  (setf (base-stream xp) stream)
  (setf (linel xp) (max 0 (cond (*print-right-margin*)
				((output-width stream))
				(T *default-right-margin*))))
  (setf (line-limit xp) *print-lines*)
  (setf (line-no xp) 1)
  (setf (char-mode xp) nil)
  (setf (char-mode-counter xp) 0)
  (setf (depth-in-blocks xp) 0)
  (setf (block-stack-ptr xp) 0)
  (setf (charpos xp) (cond ((output-position stream)) (T 0)))
  (setf (section-start xp) 0)
  (setf (buffer-ptr xp) 0)
  (setf (buffer-offset xp) (charpos xp))
  (setf (Qleft xp) 0)
  (setf (Qright xp) #.(- queue-entry-size))
  (setf (prefix-stack-ptr xp) #.(- prefix-stack-entry-size))
  xp)

;;; The char-mode stuff is a bit tricky.
;;; one can be in one of the following modes:
;;; NIL no changes to characters output.
;;; :UP CHAR-UPCASE used.
;;; :DOWN CHAR-DOWNCASE used.
;;; :CAP0 capitalize next alphanumeric letter then switch to :DOWN.
;;; :CAP1 capitalize next alphanumeric letter then switch to :CAPW
;;; :CAPW downcase letters.  When a word break letter found, switch to :CAP1.
;;; It is possible for ~(~) to be nested in a format string, but note that
;;; each mode specifies what should happen to every letter.  Therefore, inner
;;; nested modes never have any effect.  You can just ignore them.

(defun push-char-mode (xp new-mode)
  (if (zerop (char-mode-counter xp))
      (setf (char-mode xp) new-mode))
  (incf (char-mode-counter xp)))

(defun pop-char-mode (xp)
  (decf (char-mode-counter xp))
  (if (zerop (char-mode-counter xp))
      (setf (char-mode xp) nil)))

;;; Assumes is only called when char-mode is non-nil
(defun handle-char-mode (xp char)
  (case (char-mode xp)
    (:CAP0 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :DOWN) (char-upcase char))))
    (:CAP1 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :CAPW) (char-upcase char))))
    (:CAPW (cond ((alphanumericp char) (char-downcase char))
		 (T (setf (char-mode xp) :CAP1) char)))
    (:UP (char-upcase char))
    (T (char-downcase char)))) ;:DOWN

;;; All characters output are passed through the handler above.
;;; However, it must
;;; be noted that on-each-line prefixes are only processed
;;; in the context of the
;;; first place they appear.  They stay the same later no matter what.  Also
;;; non-literal newlines do not count as word breaks.
;;; This handles the basic outputting of characters.  note + suffix means that
;;; the stream is known to be an XP stream, all inputs are mandatory, and no
;;; error checking has to be done.  Suffix ++ additionally means that the
;;; output is guaranteed not to contain a newline char.
(defun write-char+ (char xp)
  (if (eql char #\newline) (pprint-newline+ :unconditional xp)
      (write-char++ char xp)))

(defun write-string+ (string xp start end)
  (let ((sub-end nil) next-newline)
    (loop (setq next-newline
		(position #\newline string
			  :test #'char= :start start :end end))
	  (setq sub-end (if next-newline next-newline end))
	  (write-string++ string xp start sub-end)
	  (when (null next-newline) (return nil))
	  (pprint-newline+ :unconditional xp)
	  (setq start (1+ sub-end)))))

;;; note this checks (> BUFFER-PTR LINEL) instead of (> (LP<-BP) LINEL)
;;; this is important so that when things are longer than a line they
;;; end up getting printed in chunks of size LINEL.

(defun write-char++ (char xp)
  (when (> (buffer-ptr xp) (linel xp))
    (force-some-output xp))
  (let ((new-buffer-end (1+ (buffer-ptr xp))))
    (check-size xp buffer new-buffer-end)
    (if (char-mode xp) (setq char (handle-char-mode xp char)))
    (setf (char (buffer xp) (buffer-ptr xp)) char)    
    (setf (buffer-ptr xp) new-buffer-end)))

(defun force-some-output (xp)
  (attempt-to-output xp nil nil)
  (when (> (buffer-ptr xp) (linel xp)) ;only if printing off end of line
    (attempt-to-output xp T T)))

(defun write-string++ (string xp start end)
  (when (> (buffer-ptr xp) (linel xp))
    (force-some-output xp))
  (write-string+++ string xp start end))

;;; never forces output; therefore safe to call from within output-line.
(defun write-string+++ (string xp start end) 
  (let ((new-buffer-end (+ (buffer-ptr xp) (- end start))))
    (check-size xp buffer new-buffer-end)
    (do ((buffer (buffer xp))
	 (i (buffer-ptr xp) (1+ i))
	 (j start (1+ j)))
	((= j end))
      (let ((char (char string j)))
	(if (char-mode xp) (setq char (handle-char-mode xp char)))
	(setf (char buffer i) char)))
    (setf (buffer-ptr xp) new-buffer-end)))

(defun pprint-tab+ (kind colnum colinc xp)
  (let ((indented? nil) (relative? nil))
    (case kind
      (:section (setq indented? T))
      (:line-relative (setq relative? T))
      (:section-relative (setq indented? T relative? T)))
    (let* ((current
	    (if (not indented?) (LP<-BP xp)
		(- (TP<-BP xp) (section-start xp))))
	   (new
	    (if (zerop colinc)
		(if relative? (+ current colnum) (max colnum current))
		(cond (relative?
		       (* colinc (floor (+ current colnum colinc -1) colinc)))
		      ((> colnum current) colnum)
		      (T (+ colnum
			    (* colinc
			       (floor (+ current (- colnum) colinc)
				      colinc)))))))
	   (length (- new current)))
      (when (plusp length)
	(if (char-mode xp) (handle-char-mode xp #\space))
	(let ((end (+ (buffer-ptr xp) length)))
	  (check-size xp buffer end)
	  (fill (buffer xp) #\space :start (buffer-ptr xp) :end end)
	  (setf (buffer-ptr xp) end))))))

;;; Note following is smallest number >= x that is a multiple of colinc
;;;  (* colinc (floor (+ x (1- colinc)) colinc))
(defun pprint-newline+ (kind xp)
  (enqueue xp :newline kind)
  (do ((ptr (Qleft xp) (Qnext ptr)))    ;find sections we are ending
      ((not (< ptr (Qright xp))))	;all but last
    (when (and (null (Qend xp ptr))
	       (not (> (depth-in-blocks xp) (Qdepth xp ptr)))
	       (member (Qtype xp ptr) '(:newline :start-block)))
      (setf (Qend xp ptr) (- (Qright xp) ptr))))
  (setf (section-start xp) (TP<-BP xp))
  (when (and (member kind '(:fresh :unconditional)) (char-mode xp))
    (handle-char-mode xp #\newline))
  (when (member kind '(:fresh :unconditional :mandatory))
    (attempt-to-output xp T nil)))

(defun start-block (xp prefix-string on-each-line? suffix-string)
  (when prefix-string
    (write-string++ prefix-string xp 0 (length prefix-string)))
  (if (and (char-mode xp) on-each-line?)
      (setq prefix-string
	    (subseq (buffer xp) (- (buffer-ptr xp) (length prefix-string))
		    (buffer-ptr xp))))
  (push-block-stack xp)
  (enqueue xp :start-block nil
	   (if on-each-line? (cons suffix-string prefix-string) suffix-string))
  (incf (depth-in-blocks xp))		;must be after enqueue
  (setf (section-start xp) (TP<-BP xp)))

(defun end-block (xp suffix)
  (unless (eq *abbreviation-happened* '*print-lines*)
    (when suffix (write-string+ suffix xp 0 (length suffix)))
    (decf (depth-in-blocks xp))
    (enqueue xp :end-block nil suffix)
    (do ((ptr (Qleft xp) (Qnext ptr)))	; looking for start of block
					; we are ending		
	((not (< ptr (Qright xp))))	;all but last
      (when (and (= (depth-in-blocks xp) (Qdepth xp ptr))
		 (eq (Qtype xp ptr) :start-block)
		 (null (Qoffset xp ptr)))
	(setf (Qoffset xp ptr) (- (Qright xp) ptr))
	(return nil)))			;can only be 1
    (pop-block-stack xp)))

(defun pprint-indent+ (kind n xp)
  (enqueue xp :ind kind n))

;;; The next function scans the queue looking for things it can do.
;;; it keeps outputting things until the queue is empty, or it finds
;;;  a place where it cannot make a decision yet.

(defmacro maybe-too-large (xp Qentry)
  `(let ((limit (linel ,xp)))
    (when (eql (line-limit ,xp) (line-no ,xp)) ;prevents suffix overflow
      (decf limit 2) ;3 for " .." minus 1 for space (heuristic)
      (when (not (minusp (prefix-stack-ptr ,xp)))
	(decf limit (suffix-ptr ,xp))))
    (cond ((Qend ,xp ,Qentry)
	   (> (LP<-TP ,xp (Qpos ,xp (+ ,Qentry (Qend ,xp ,Qentry)))) limit))
	  ((or force-newlines? (> (LP<-BP ,xp) limit)) T)
	  (T (return nil)))))	;wait until later to decide.

(defmacro misering? (xp)
  `(and *print-miser-width*
    (<= (- (linel ,xp) (initial-prefix-ptr ,xp)) *print-miser-width*)))

;;; If flush-out? is T and force-newlines? is NIL then the buffer,
;;; prefix-stack, and queue will be in an inconsistent state after the call.
;;; You better not call it this way except as the last act of outputting.
(defun attempt-to-output (xp force-newlines? flush-out?)
  (do () ((> (Qleft xp) (Qright xp))
	  (setf (Qleft xp) 0)
	  (setf (Qright xp) #.(- queue-entry-size))) ;saves shifting
    (case (Qtype xp (Qleft xp))
      (:ind
       (unless (misering? xp)
	 (set-indentation-prefix
	  xp
	  (case (Qkind xp (Qleft xp))
	    (:block (+ (initial-prefix-ptr xp) (Qarg xp (Qleft xp))))
	    (T				; :current
	     (+ (LP<-TP xp (Qpos xp (Qleft xp)))
		(Qarg xp (Qleft xp)))))))
       (setf (Qleft xp) (Qnext (Qleft xp))))
      (:start-block
       (cond ((maybe-too-large xp (Qleft xp))
	      (push-prefix-stack xp)
	      (setf (initial-prefix-ptr xp) (prefix-ptr xp))
	      (set-indentation-prefix xp (LP<-TP xp (Qpos xp (Qleft xp))))
	      (let ((arg (Qarg xp (Qleft xp))))
		(when (consp arg) (set-prefix xp (cdr arg)))
		(setf (initial-prefix-ptr xp) (prefix-ptr xp))
		(cond ((not (listp arg)) (set-suffix xp arg))
		      ((car arg) (set-suffix xp (car arg)))))
	      (setf (section-start-line xp) (line-no xp)))
	     (T (incf (Qleft xp) (Qoffset xp (Qleft xp)))))
       (setf (Qleft xp) (Qnext (Qleft xp))))
      (:end-block (pop-prefix-stack xp) (setf (Qleft xp) (Qnext (Qleft xp))))
      (T				; :newline
       (when (case (Qkind xp (Qleft xp))
	       (:fresh (not (zerop (LP<-BP xp))))
	       (:miser (misering? xp))
	       (:fill (or (misering? xp)
			  (> (line-no xp) (section-start-line xp))
			  (maybe-too-large xp (Qleft xp))))
	       (T T))			;(:linear :unconditional :mandatory) 
	 (output-line xp (Qleft xp))
	 (setup-for-next-line xp (Qleft xp)))
       (setf (Qleft xp) (Qnext (Qleft xp))))))
  (when flush-out? (flush xp)))

;;; this can only be called last!
(defun flush (xp)
  (unless *locating-circularities*
    (lisp:write-string
     (buffer xp) (base-stream xp) :end (buffer-ptr xp)))
  (incf (buffer-offset xp) (buffer-ptr xp))
  (incf (charpos xp) (buffer-ptr xp))
  (setf (buffer-ptr xp) 0))

;;; This prints out a line of stuff.
(defun output-line (xp Qentry)
  (let* ((out-point (BP<-TP xp (Qpos xp Qentry)))
	 (last-non-blank (position #\space (buffer xp)
				   :test-not #'char=
				   :from-end T
				   :end out-point))
	 (end (cond ((member (Qkind xp Qentry) '(:fresh :unconditional))
		     out-point)
		    (last-non-blank (1+ last-non-blank))
		    (T 0)))
	 (line-limit-exit (and (line-limit xp)
			       (not (> (line-limit xp) (line-no xp))))))
    (when line-limit-exit
      (setf (buffer-ptr xp) end)	; truncate pending output.
      (write-string+++ " .." xp 0 3)
      (reverse-string-in-place (suffix xp) 0 (suffix-ptr xp))
      (write-string+++ (suffix xp) xp 0 (suffix-ptr xp))
      (setf (Qleft xp) (Qnext (Qright xp)))
      (setq *abbreviation-happened* '*print-lines*)
      (throw 'line-limit-abbreviation-exit T))
    (incf (line-no xp))
    (unless *locating-circularities*
      (lisp:write-line
       (buffer xp) (base-stream xp) :end end))))

(defun setup-for-next-line (xp Qentry)
  (let* ((out-point (BP<-TP xp (Qpos xp Qentry)))
	 (prefix-end
	  (cond ((member (Qkind xp Qentry) '(:unconditional :fresh))
		 (non-blank-prefix-ptr xp))
		(T (prefix-ptr xp))))
	 (change (- prefix-end out-point)))
    (setf (charpos xp) 0)
    (when (plusp change)                  ;almost never happens
      (check-size xp buffer (+ (buffer-ptr xp) change)))
    (replace (buffer xp) (buffer xp) :start1 prefix-end
	     :start2 out-point :end2 (buffer-ptr xp))
    (replace (buffer xp) (prefix xp) :end2 prefix-end)
    (incf (buffer-ptr xp) change)
    (decf (buffer-offset xp) change)
    (when (not (member (Qkind xp Qentry) '(:unconditional :fresh)))
      (setf (section-start-line xp) (line-no xp)))))

(defun set-indentation-prefix (xp new-position)
  (let ((new-ind (max (non-blank-prefix-ptr xp) new-position)))
    (setf (prefix-ptr xp) (initial-prefix-ptr xp))
    (check-size xp prefix new-ind)
    (when (> new-ind (prefix-ptr xp))
      (fill (prefix xp) #\space :start (prefix-ptr xp) :end new-ind))
    (setf (prefix-ptr xp) new-ind)))

(defun set-prefix (xp prefix-string)
  (replace (prefix xp) prefix-string
	   :start1 (- (prefix-ptr xp) (length prefix-string)))
  (setf (non-blank-prefix-ptr xp) (prefix-ptr xp)))

(defun set-suffix (xp suffix-string)
  (let* ((end (length suffix-string))
	 (new-end (+ (suffix-ptr xp) end)))
    (check-size xp suffix new-end)
    (do ((i (1- new-end) (1- i)) (j 0 (1+ j))) ((= j end))
      (setf (char (suffix xp) i) (char suffix-string j)))
    (setf (suffix-ptr xp) new-end)))

(defun reverse-string-in-place (string start end)
  (do ((i start (1+ i))
       (j (1- end) (1- j)))
      ((not (< i j)) string)
    (let ((c (char string i)))
      (setf (char string i) (char string j))
      (setf (char string j) c))))

