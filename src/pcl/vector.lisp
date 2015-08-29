;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;
;;; Permutation vectors.
;;;

(in-package 'pcl)

(defmacro instance-slot-index (wrapper slot-name)
  `(let ((pos 0))
     (block loop
       (dolist (sn (wrapper-instance-slots-layout ,wrapper))
	 (when (eq ,slot-name sn) (return-from loop pos))
	 (incf pos)))))


;;;
;;;
;;;
(defmacro %isl-cache           (isl) `(%svref ,isl 1))
(defmacro %isl-field           (isl) `(%svref ,isl 2))
(defmacro %isl-mask            (isl) `(%svref ,isl 3))
(defmacro %isl-size            (isl) `(%svref ,isl 4))
(defmacro %isl-slot-name-lists (isl) `(%svref ,isl 5))

(defun make-isl (slot-name-lists)
  (multiple-value-bind (mask size)
      (compute-primary-pv-cache-size slot-name-lists)
    (make-isl-internal (wrapper-field 'number)
		       (get-cache size)
		       mask
		       size
		       slot-name-lists)))

(defun make-isl-internal (field cache mask size slot-name-lists)  
  (let ((isl (make-array 6)))
    (setf (svref isl 0)               'isl
	  (%isl-cache isl)            cache
	  (%isl-field isl)            field
	  (%isl-mask  isl)            mask
	  (%isl-size  isl)            size
	  (%isl-slot-name-lists isl)  slot-name-lists)
    isl))

(defun make-isl-type-declaration (var)
  `(type simple-vector ,var))

(defun islp (x)
  (and (simple-vector-p x)
       (= (array-dimension x 0) 5)
       (eq (svref x 0) 'isl)))

(defvar *slot-name-lists-inner* (make-hash-table :test #'equal))
(defvar *slot-name-lists-outer* (make-hash-table :test #'equal))

(defun intern-slot-name-lists (slot-name-lists)
  (flet ((inner (x) 
	   (or (gethash x *slot-name-lists-inner*)
	       (setf (gethash x *slot-name-lists-inner*) (copy-list x))))
	 (outer (x) 
	   (or (gethash x *slot-name-lists-outer*)
	       (setf (gethash x *slot-name-lists-outer*) (make-isl (copy-list x))))))
    (outer (mapcar #'inner slot-name-lists))))



(defvar *pvs* (make-hash-table :test #'equal))

(defvar *original-methods-table* (make-hash-table :test #'eq))

(defun optimize-method-call-p (gfun-name types original-method-spec)
  (let ((gfun (gdefinition gfun-name)))
    (multiple-value-bind (applicable-methods possibly-applicable-methods)
	(compute-applicable-methods-using-types gfun types)
      (and applicable-methods (null (cdr applicable-methods))
	   (null possibly-applicable-methods)
	   (eq (car applicable-methods)
	       (or (gethash gfun *original-methods-table*)
		   (setf (gethash gfun *original-methods-table*)
			 (get-method gfun nil
				     (mapcar #'find-class original-method-spec)))))))))

(defun optimize-slot-value-by-class-p (class slot-name operation)
  (or (not (eq *boot-state* 'complete))
      (let ((slot-definition (find-slot-definition class slot-name)))
	(multiple-value-bind (gfun-name setf-p)
	    (case operation
	      (slot-value     (values 'slot-value-using-class nil))
	      (set-slot-value (values '(setf slot-value-using-class) t))
	      (slot-boundp    (values 'slot-boundp-using-class nil)))
	  (flet ((add-value-type (list) 
		   (if setf-p (cons 't list) list)))
	    (optimize-method-call-p
	     gfun-name
	     (add-value-type `((eql ,class) (class-eq ,class) (eql ,slot-definition)))
	     (add-value-type '(std-class 
			       standard-object
			       standard-effective-slot-definition))))))))

(defun lookup-pv (isl args)
  (let* ((class-slot-p nil)
	 (elements
	  (gathering1 (collecting)
	    (iterate ((slot-names (list-elements (%isl-slot-name-lists isl)))
		      (arg (list-elements args)))
	      (when slot-names
		(let* ((wrapper     (check-wrapper-validity arg))
		       (class       (wrapper-class wrapper))
		       (class-slots (wrapper-class-slots wrapper)))
		  (dolist (slot-name slot-names)
		    (if (dolist (op '(slot-value set-slot-value slot-boundp) t)
			  (unless (optimize-slot-value-by-class-p class slot-name op)
			    (return nil)))
			(let ((index (instance-slot-index wrapper slot-name)))
			  (if index
			      (gather1 index)
			      (let ((cell (assq slot-name class-slots)))
				(if cell
				    (progn (setq class-slot-p t) (gather1 cell))
				    (gather1 nil)))))
			(gather1 nil)))))))))
    (if class-slot-p				;Sure is a shame Common Lisp doesn't
	(make-permutation-vector elements)	;give me the right kind of hash table.
	(or (gethash elements *pvs*)
	    (setf (gethash elements *pvs*) (make-permutation-vector elements))))))

(defun make-permutation-vector (indexes)
  (make-array (length indexes) :initial-contents indexes))

(defun make-pv-type-declaration (var)
  `(type simple-vector ,var))

(defmacro pvref (pv index)
  `(svref ,pv ,index))



(defun can-optimize-access (var required-parameters env)
  (let ((rebound? (caddr (variable-declaration 'variable-rebinding var env))))
    (if rebound?
	(car (memq rebound? required-parameters))
	(car (memq var required-parameters)))))

(defun optimize-slot-value (slots parameter form)
  (destructuring-bind (ignore ignore slot-name)
		      form
    (optimize-instance-access slots :read parameter (eval slot-name) nil)))

(defun optimize-set-slot-value (slots parameter form)
  (destructuring-bind (ignore ignore slot-name new-value)
		      form
    (optimize-instance-access slots :write parameter (eval slot-name) new-value)))

(defun optimize-slot-boundp (slots parameter form)
  (destructuring-bind (ignore ignore slot-name new-value)
		      form
    (optimize-instance-access slots :boundp parameter (eval slot-name) new-value)))

;;;
;;; The <slots> argument is an alist, the CAR of each entry is the name of
;;; a required parameter to the function.  The alist is in order, so the
;;; position of an entry in the alist corresponds to the argument's position
;;; in the lambda list.
;;; 
(defun optimize-instance-access (slots read/write parameter slot-name new-value)
  (let* ((parameter-entry (assq parameter slots))
	 (slot-entry      (assq slot-name (cdr parameter-entry)))
	 (position (position parameter-entry slots)))
    (unless parameter-entry
      (error "Internal error in slot optimization."))
    (unless slot-entry
      (setq slot-entry (list slot-name))
      (push slot-entry (cdr parameter-entry)))
    (ecase read/write
      (:read
	(let ((form (list 'instance-read ''.PV-OFFSET. parameter position 
			  `',slot-name)))
	  (push form (cdr slot-entry))
	  form))
      (:write
	(let ((form (list 'instance-write ''.PV-OFFSET. parameter position 
			  `',slot-name '.new-value.)))
	  (push form (cdr slot-entry))
	  `(let ((.new-value. ,new-value)) ,form)))
      (:boundp
	(let ((form (list 'instance-boundp ''.PV-OFFSET. parameter position 
			  `',slot-name)))
	  (push form (cdr slot-entry))
	  form)))))

(define-walker-template instance-read)
(define-walker-template instance-write)
(define-walker-template instance-boundp)


(defmacro instance-read (pv-offset parameter position slot-name)
  `(locally
     (declare (optimize (speed 3) (safety 0) (compilation-speed 0)))
     (let ((.INDEX. (pvref .PV. ,pv-offset)))
       (if (and (typep .INDEX. 'fixnum)
		(neq (setq .INDEX. (%svref ,(slot-vector-symbol position) .INDEX.))
		     ',*slot-unbound*))
	   .INDEX.
	   (if (consp .INDEX.)
	       (cdr .INDEX.)
	       (funcall #'slot-value ,parameter ,slot-name))))))

(defmacro instance-write (pv-offset parameter position slot-name new-value)
  `(locally
     (declare (optimize (speed 3) (safety 0) (compilation-speed 0)))
     (let ((.INDEX. (pvref .PV. ,pv-offset)))
       (if (typep .INDEX. 'fixnum)
	   (setf (%svref ,(slot-vector-symbol position) .INDEX.) ,new-value)
	   (if (consp .INDEX.)
	       (setf (cdr .INDEX.) ,new-value)
	       (funcall #'set-slot-value ,parameter ,slot-name ,new-value))))))

(defmacro instance-boundp (pv-offset parameter position slot-name)
  `(locally
     (declare (optimize (speed 3) (safety 0) (compilation-speed 0)))
     (let ((.INDEX. (pvref .PV. ,pv-offset)))
       (if (typep .INDEX. 'fixnum)
	   (neq (setq .INDEX. (%svref ,(slot-vector-symbol position) .INDEX.))
		',*slot-unbound*)
	   (if (consp .INDEX.)
	       (neq (cdr .INDEX.) ',*slot-unbound*)
	       (funcall #'slot-boundp ,parameter ,slot-name))))))

;;;
;;; This magic function has quite a job to do indeed.
;;;
;;; The careful reader will recall that <slots> contains all of the optimized
;;; slot access forms produced by OPTIMIZE-INSTANCE-ACCESS.  Each of these is
;;; a call to either INSTANCE-READ or INSTANCE-WRITE.
;;;
;;; At the time these calls were produced, the first argument was specified as
;;; the symbol .PV-OFFSET.; what we have to do now is convert those pv-offset
;;; arguments into the actual number that is the correct offset into the pv.
;;;
;;; But first, oh but first, we sort <slots> a bit so that for each argument
;;; we have the slots in alphabetical order.  This canonicalizes the ISL's a
;;; bit and will hopefully lead to having fewer PV's floating around.  Even
;;; if the gain is only modest, it costs nothing.
;;;  
(defun slot-name-lists-from-slots (slots)
  (mapcar #'(lambda (parameter-entry) (mapcar #'car (cdr parameter-entry)))
	  (mutate-slots slots)))

(defun mutate-slots (slots)
  (let ((sorted (sort-slots slots))
	(pv-offset -1))
    (dolist (parameter-entry sorted)
      (dolist (slot-entry (cdr parameter-entry))
	(incf pv-offset)	
	(dolist (form (cdr slot-entry))
	  (setf (cadr form) pv-offset))))
    sorted))

(defun sort-slots (slots)
  (mapcar #'(lambda (parameter-entry)
	      (cons (car parameter-entry)
		    (sort (cdr parameter-entry)	;slot entries
			  #'(lambda (a b)
			      (string-lessp (symbol-name (car a))
					    (symbol-name (car b)))))))
	  slots))


;;;
;;; This needs to work in terms of metatypes and also needs to work for
;;; automatically generated reader and writer functions.
;;;   
(defun add-pv-binding (method-body plist required-parameters)
  (let* ((isl (getf plist :isl))
	 (isl-cache-symbol (wcl-make-symbol "isl-cache")))
    (nconc plist (list :isl-cache-symbol isl-cache-symbol))
    (with-gathering ((slot-variables (collecting))
		     (metatypes (collecting)))
	  (iterate ((slots (list-elements isl))
		    (i (interval :from 0)))
	    (cond (slots
		   (gather (slot-vector-symbol i) slot-variables)
		   (gather 'standard-instance     metatypes))
		  (t
		   (gather nil slot-variables)
		   (gather t   metatypes))))
      `((let ((.ISL. (locally (declare (special ,isl-cache-symbol)) ,isl-cache-symbol))
	      (.PV. *empty-vector*)
	      ,@(remove nil slot-variables))
	  (declare ,(make-isl-type-declaration '.ISL.)
		   ,(make-pv-type-declaration '.PV.))
	
	  (let* ((cache (%isl-cache .ISL.))
		 (size  (%isl-size  .ISL.))
		 (mask  (%isl-mask  .ISL.))
		 (field (%isl-field .ISL.)))
	    ,(generating-lap-in-lisp '(cache size mask field)
				     required-parameters
	       (flatten-lap
		 (emit-pv-dlap required-parameters metatypes slot-variables))))

	  ,@method-body)))))

(defun emit-pv-dlap (required-parameters metatypes slot-variables)
  (let* ((slot-regs (mapcar #'(lambda (sv) (and sv (operand :lisp-variable sv)))
			    slot-variables))
	 (wrappers (dlap-wrappers metatypes))
	 (nwrappers (remove nil wrappers)))
    (flet ((wrapper-moves (miss-label)
	     (dlap-wrapper-moves wrappers required-parameters metatypes miss-label slot-regs)))
      (prog1 (emit-dlap-internal
	       nwrappers                   ;wrapper-regs
	       (wrapper-moves 'pv-miss)    ;wrapper-moves
	       (opcode :exit-lap-in-lisp)  ;hit
	       (flatten-lap		   ;miss
		 (opcode :label 'pv-miss)
		 (opcode :move
			 (operand :lisp `(primary-pv-cache-miss
					  .ISL. ,@required-parameters))
			 (operand :lisp-variable '.PV.))
		 (apply #'flatten-lap (wrapper-moves 'pv-wrapper-miss)) ; -- Maybe the wrappers have changed.
		 (opcode :label 'pv-wrapper-miss)
		 (opcode :exit-lap-in-lisp))                               
	       'pv-miss			    ;miss-label
	       (operand :lisp-variable '.PV.)) ;value-reg
	   (mapc #'deallocate-register nwrappers)))))

(defun compute-primary-pv-cache-size (slot-name-lists)
  (compute-cache-parameters (- (length slot-name-lists) (count nil slot-name-lists))
			    t
			    2))

(defun pv-cache-limit-fn (nlines)
  (default-limit-fn nlines))

(defun primary-pv-cache-miss (isl &rest args)
  (let* ((wrappers
	   (gathering1 (collecting) 
	     (iterate ((slot-names (list-elements (%isl-slot-name-lists isl)))
		       (arg        (list-elements args)))
	       (when slot-names (gather1 (check-wrapper-validity arg))))))
	 (pv (lookup-pv isl args))
	 (field (%isl-field isl))
	 (cache (%isl-cache isl))
	 (nkeys (length wrappers)))
    (multiple-value-bind (new-field new-cache new-mask new-size)
	(fill-cache field cache nkeys t #'pv-cache-limit-fn
		    (if (= nkeys 1) (car wrappers) wrappers)
		    pv)
      (when (or (not (= new-field field))
		(not (eq new-cache cache)))
	(without-interrupts			;NOTE:
	  (setf (%isl-field isl) new-field	; There is no mechanism to
		(%isl-cache isl) new-cache	; synchronize the reading of
		(%isl-size  isl) new-size	; these values.  But, this is
		(%isl-mask  isl) new-mask))	; a safe order to write them
						; in.  Stricly speaking, the
						; use of without-interrupts
						; is superfluous.
	(when (neq new-cache cache) (free-cache cache))))
    pv))



(defmethod wrapper-fetcher ((class standard-class))
  'std-instance-wrapper)

(defmethod slots-fetcher ((class standard-class))
  'std-instance-slots)

(defmethod raw-instance-allocator ((class standard-class))
  '%%allocate-instance--class)

