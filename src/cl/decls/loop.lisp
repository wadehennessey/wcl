;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;;   -*- Mode:LISP; Syntax:Common-Lisp; Package:LOOP; Base:10; Lowercase:T -*-
;;;   *************************************************************************
;;;   ******* Common Lisp ******** LOOP Iteration Macro ***********************
;;;   *************************************************************************
;;;   ***** (C) COPYRIGHT 1980, 1981 MASSACHUSETTS INSTITUTE OF TECHNOLOGY ****
;;;   ********* THIS IS A READ-ONLY FILE! (ALL WRITES RESERVED) ***************
;;;   *************************************************************************

;;;; LOOP Iteration Macro

;;; This is the "officially sanctioned" version of LOOP for running in
;;; Common Lisp.  It is a conversion of LOOP 829, which is fairly close to
;;; that released with Symbolics Release 6.1 (803).  This conversion was
;;; made by Glenn Burke (one of the original author/maintainers);  the
;;; work was performed at Palladian Software, in Cambridge MA, April 1986.
;;; 
;;; The current version of this file will be maintained at MIT, available
;;; for anonymous FTP on MC.LCS.MIT.EDU from the file "LSB1;CLLOOP >".  This
;;; location will no doubt change sometime in the future.
;;; 
;;; This file was actually taken from ai.ai.mit.edu:gsb;clloop >
;;;
;;; This file, like the LOOP it is derived from, has unrestricted
;;; distribution -- anyone may take it and use it.  But for the sake of
;;; consistency, bug reporting, compatibility, and users' sanity, PLEASE
;;; PLEASE PLEASE don't go overboard with fixes or changes.  Remember that
;;; this version is supposed to be compatible with the Maclisp/Zetalisp/NIL
;;; LOOP;  it is NOT intended to be "different" or "better" or "redesigned".
;;; Report bugs and propose fixes to BUG-LOOP@MC.LCS.MIT.EDU;
;;; announcements about LOOP will be made to the mailing list
;;; INFO-LOOP@MC.LCS.MIT.EDU.  Mail concerning those lists (such as requests
;;; to be added) should be sent to the BUG-LOOP-REQUEST and
;;; INFO-LOOP-REQUEST lists respectively.  Note the Change History page
;;; below...
;;; 
;;; LOOP documentation is still probably available from the MIT Laboratory
;;; for Computer Science publications office:
;;; 	LCS Publications
;;; 	545 Technology Square
;;; 	Cambridge, MA 02139
;;; It is Technical Memo 169, "LOOP Iteration Macro", and is very old.  The
;;; most up-to-date documentation on this version of LOOP is that in the NIL
;;; Reference Manual (TR-311 from LCS Publications);  while you wouldn't
;;; want to get that (it costs nearly $15) just for LOOP documentation,
;;; those with access to a NIL manual might photocopy the chapter on LOOP.
;;; That revised documentation can be reissued as a revised technical memo
;;; if there is sufficient demand.
;;;


;;;; Change History

;;; [gsb@palladian] 30-apr-86 00:26  File Created from NIL's LOOP version 829
;;; [gsb@palladian] 30-oct-86 18:23  don't generate (type notype var) decls, special-case notype into T.
;;;		    (The NOTYPE type keyword needs to be around for compatibility.)
;;; [gsb@palladian] 30-oct-86 18:48  bogus case clause in loop-do-collect.  Syntax:common-lisp in file
;;;		    attribute list, for symbolics gratuitousness.
;;;------------------------------------------------------------------------
;;;------- End of official change history -- note local fixes below -------
;;;------------------------------------------------------------------------

;;; [boyer@rascal.ics.utexas.edu] 9-july-87 In define-loop-macro moved
;;; &environment and its formal to right after &whole to overcome bug
;;; in KCL.

;;;; Macro Environment Setup

; Hack up the stuff for data-types.  DATA-TYPE? will always be a macro
; so that it will not require the data-type package at run time if
; all uses of the other routines are conditionalized upon that value.
;;; HEY! ZAPPED: (eval-when (eval compile)
  ; Crock for DATA-TYPE? derives from DTDCL.  We just copy it rather
  ; than load it in, which requires knowing where it comes from (sigh).
  ; 
(defmacro data-type? (frob)
  (let ((foo (gensym)))
    `((lambda (,foo)
	;; NIL croaks if nil given to GET...  No it doesn't any more!  But:
	;; Every Lisp should (but doesn't) croak if randomness given to GET
	;; LISPM croaks (of course) if randomness given to get-pname
	(and (symbolp ,foo)
	     (or (get ,foo ':data-type)
		 (and (setq ,foo (find-symbol (symbol-name ,foo) 'keyword))
		      (get ,foo ':data-type)))))
      ,frob)))

;;; The uses of this macro are retained in the CL version of loop, in case they are
;;; needed in a particular implementation.  Originally dating from the use of the
;;; Zetalisp COPYLIST* function, this is used in situations where, were cdr-coding
;;; in use, having cdr-NIL at the end of the list might be suboptimal because the
;;; end of the list will probably be RPLACDed and so cdr-normal should be used instead.
(defmacro loop-copylist* (l)
  `(copy-list ,l))


;;;; Random Macros

(defmacro loop-simple-error (unquoted-message &optional (datum nil datump))
  `(error ,(if datump "Loop error - ~A: ~A" "LOOP:  ~A")
	  ',unquoted-message ,@(and datump (list datum))))


(defmacro loop-warn (unquoted-message &optional (datum nil datump))
  (if datump
      `(warn ,(concatenate 'string "LOOP: " unquoted-message " -- ~{~S~^ ~}")
	     ,datum)
      `(warn ',(concatenate 'string "LOOP: " unquoted-message))))


(defmacro loop-pop-source () '(pop loop-source-code))

(defmacro loop-gentemp (&optional (pref ''loopvar-))
  `(gensym (symbol-name ,pref)))

(defvar loop-use-system-destructuring?
    nil)

(defvar loop-desetq-temporary)

; Do we want this???  It is, admittedly, useful...
;(defmacro loop-desetq (&rest x)
;  (let ((loop-desetq-temporary nil))
;     (let ((setq-form (loop-make-desetq x)))
;	(if loop-desetq-temporary
;	    `((lambda (,loop-desetq-temporary) ,setq-form) nil)
;	    setq-form))))

(defparameter loop-keyword-alist			;clause introducers
     '(	(named loop-do-named)
	(initially loop-do-initially)
	(finally loop-do-finally)
	(nodeclare loop-nodeclare)
	(do loop-do-do)
	(doing loop-do-do)
	(return loop-do-return)
	(collect loop-do-collect list)
	(collecting loop-do-collect list)
	(append loop-do-collect append)
	(appending loop-do-collect append)
	(nconc loop-do-collect nconc)
	(nconcing loop-do-collect nconc)
	(count loop-do-collect count)
	(counting loop-do-collect count)
	(sum loop-do-collect sum)
	(summing loop-do-collect sum)
	(maximize loop-do-collect max)
	(minimize loop-do-collect min)
	(always loop-do-always nil) ;Normal, do always
	(never loop-do-always t)    ; Negate the test on always.
	(thereis loop-do-thereis)
	(while loop-do-while nil while)	    ; Normal, do while
	(until loop-do-while t until)	    ; Negate the test on while
	(when loop-do-when nil when)	    ; Normal, do when
	(if loop-do-when nil if)    ; synonymous
 	(unless loop-do-when t unless)	    ; Negate the test on when
	(with loop-do-with)))

(defparameter loop-iteration-keyword-alist
    `((for loop-do-for)
      (as loop-do-for)
      (repeat loop-do-repeat)))

(defparameter loop-for-keyword-alist			;Types of FOR
     '( (= loop-for-equals)
        (first loop-for-first)
	(in loop-list-stepper car)
	(on loop-list-stepper nil)
	(from loop-for-arithmetic from)
	(downfrom loop-for-arithmetic downfrom)
	(upfrom loop-for-arithmetic upfrom)
	(below loop-for-arithmetic below)
	(to loop-for-arithmetic to)
	(being loop-for-being)))

(defvar loop-prog-names)

(defvar loop-macro-environment)	;Second arg to macro functions,
					;passed to macroexpand.

(defvar loop-path-keyword-alist nil)		; PATH functions
(defvar loop-named-variables)			; see LOOP-NAMED-VARIABLE
(defvar loop-variables)			;Variables local to the loop
(defvar loop-declarations)			; Local dcls for above
(defvar loop-nodeclare)			; but don't declare these
(defvar loop-variable-stack)
(defvar loop-declaration-stack)
(defvar loop-desetq-crocks)			; see loop-make-variable
(defvar loop-desetq-stack)			; and loop-translate-1
(defvar loop-prologue)				;List of forms in reverse order
(defvar loop-wrappers)				;List of wrapping forms, innermost first
(defvar loop-before-loop)
(defvar loop-body)				;..
(defvar loop-after-body)			;.. for FOR steppers
(defvar loop-epilogue)				;..
(defvar loop-after-epilogue)			;So COLLECT's RETURN comes after FINALLY
(defvar loop-conditionals)			;If non-NIL, condition for next form in body
					;The above is actually a list of entries of the form
					;(cond (condition forms...))
					;When it is output, each successive condition will get
					;nested inside the previous one, but it is not built up
					;that way because you wouldn't be able to tell a WHEN-generated
					;COND from a user-generated COND.
					;When ELSE is used, each cond can get a second clause

(defvar loop-when-it-variable)			;See LOOP-DO-WHEN
(defvar loop-never-stepped-variable)		; see LOOP-FOR-FIRST
(defvar loop-emitted-body?)			; see LOOP-EMIT-BODY,
					; and LOOP-DO-FOR
(defvar loop-iteration-variables)		; LOOP-MAKE-ITERATION-VARIABLE
(defvar loop-iteration-variablep)		; ditto
(defvar loop-collect-cruft)			; for multiple COLLECTs (etc)
(defvar loop-source-code)
(defvar loop-duplicate-code nil)  ; see LOOP-OPTIMIZE-DUPLICATED-CODE-ETC

(defmacro define-loop-macro (keyword)
  "Makes KEYWORD, which is a LOOP keyword, into a Lisp macro that may
introduce a LOOP form.  This facility exists mostly for diehard users of
a predecessor of LOOP.  Unconstrained use is not advised, as it tends to
decrease the transportability of the code and needlessly uses up a
function name."
  (or (eq keyword 'loop)
      (loop-tassoc keyword loop-keyword-alist)
      (loop-tassoc keyword loop-iteration-keyword-alist)
      (loop-simple-error "not a loop keyword - define-loop-macro" keyword))
  `(defmacro ,keyword (&whole whole-form  &environment env &rest keywords-and-forms)
     ;; W can't hack this declare yet...
     ;; (declare (ignore keywords-and-forms))
     (loop-translate whole-form env)))


(define-loop-macro loop)


(defmacro loop-finish () 
  "Causes the iteration to terminate \"normally\", the same as implicit
termination by an iteration driving clause, or by use of WHILE or
UNTIL -- the epilogue code (if any) will be run, and any implicitly
collected result will be returned as the value of the LOOP."
  '(go end-loop))


(defvar loop-floating-point-types
	'(flonum float short-float single-float double-float long-float))


























(defvar loop-simplep
	'(> < <= >= /= + - 1+ 1- ash equal atom setq prog1 prog2 and or = aref char schar sbit svref))
















(defmacro define-loop-path (names &rest cruft)
  "(DEFINE-LOOP-PATH NAMES PATH-FUNCTION LIST-OF-ALLOWABLE-PREPOSITIONS
DATUM-1 DATUM-2 ...)
Defines PATH-FUNCTION to be the handler for the path(s) NAMES, which may
be either a symbol or a list of symbols.  LIST-OF-ALLOWABLE-PREPOSITIONS
contains a list of prepositions allowed in NAMES. DATUM-i are optional;
they are passed on to PATH-FUNCTION as a list."
  (setq names (if (atom names) (list names) names))
  (let ((forms (mapcar #'(lambda (name) `(loop-add-path ',name ',cruft))
		       names)))
    `(eval-when (eval load compile) ,@forms)))


(defmacro define-loop-sequence-path (path-name-or-names fetchfun sizefun
				     &optional sequence-type element-type)
  "Defines a sequence iiteration path.  PATH-NAME-OR-NAMES is either an
atomic path name or a list of path names.  FETCHFUN is a function of
two arguments, the sequence and the index of the item to be fetched.
Indexing is assumed to be zero-origined.  SIZEFUN is a function of
one argument, the sequence; it should return the number of elements in
the sequence.  SEQUENCE-TYPE is the name of the data-type of the
sequence, and ELEMENT-TYPE is the name of the data-type of the elements
of the sequence."
    `(define-loop-path ,path-name-or-names
	loop-sequence-elements-path
	(of in from downfrom to downto below above by)
	,fetchfun ,sizefun ,sequence-type ,element-type))

;;;; Setup stuff
(mapc #'(lambda (x) 
	  (mapc #'(lambda (y)
		    (setq loop-path-keyword-alist
			  (cons `(,y loop-sequence-elements-path
				  (of in from downfrom to downto
				      below above by)
				  ,@(cdr x))
				(delete (loop-tassoc
					  y loop-path-keyword-alist)
					loop-path-keyword-alist
					:test #'eq :count 1))))
		(car x)))
      '( ((element elements) elt length sequence)
	;The following should be done by using ELEMENTS and type dcls...
	  ((vector-element 
	    vector-elements 
	    array-element    ;; Backwards compatibility -- DRM
	    array-elements)
	   aref length vector)
	  ((simple-vector-element simple-vector-elements
	    simple-general-vector-element simple-general-vector-elements)
	   svref simple-vector-length simple-vector)
	  ((bits bit bit-vector-element bit-vector-elements)
	     bit bit-vector-length bit-vector bit)
	  ((simple-bit-vector-element simple-bit-vector-elements)
	     sbit simple-bit-vector-length simple-bit-vector bit)
	  ((character characters string-element string-elements)
	   char string-length string string-char)
	  ((simple-string-element simple-string-elements)
	   schar simple-string-length simple-string string-char)
	)
      )


