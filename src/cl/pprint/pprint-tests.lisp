;-*- Syntax:COMMON-LISP;Default-character-style: (:FIX :BOLD :LARGE)-*-

;------------------------------------------------------------------------

;Copyright 1989,1990 by the Massachusetts Institute of Technology, Cambridge, 
;Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------

;  This is a file of test cases to test XP.  Just load it and run the
;function (DO-TESTS).  It prints out identifying numbers of tests
;as it performs them one after another.  When all of the tests have
;been run, a summary line is printed saying how many tests failed.

;  Whenever a test fails for any reason, an error is signalled.  To
;continue testing, call the function (MORE) either within the break,
;or at top level after aborting the execution of a test.  (The latter
;is useful if a test leads to an infinite loop.)  When all of the
;tests have been completed, the variable FAILED-TESTS contains a list
;of the numbers of the tests that failed.  (You can look at the tests
;themselves by evaluating (NTH N TEST-LIST) for any test number.)

;  After running the tests and fixing problems which arise, you may wish
;to run some or all of the tests again.  Calling (DO-TESTS) runs all
;of the tests again.  Calling (DO-FAILED-TESTS) runs just the tests
;which failed the first time.  (The variable TESTS-FAILED is updated
;to reflect the new state of affairs in either case.)  Calling
;(DO-TEST n) runs just the test with the given number. 

#+:symbolics(use-package "CL")
(in-package "USER")
(eval-when (eval load compile) (xp::install #+symbolics :macro #+symbolics T))
(proclaim '(special form test-list failed-tests *dt*))
(defvar in-tester nil)
(defvar tests nil)
(defvar compile-tests T)

(defun do-tests ()
  (setq xp::*format-string-cache* T)
  (lisp:format T "~% Running the suite of ~S test cases~%" (length test-list))
  (setq tests (do ((i (1- (length test-list)) (1- i))
		   (r nil (cons i r)))
		  ((minusp i) r))
	failed-tests nil)
  (do-many-tests))

(defun do-failed-tests ()
  (lisp:format T "~% Running the ~S failed tests~%" (length failed-tests))
  (setq tests failed-tests failed-tests nil)
  (do-many-tests))

(defun do-many-tests ()
  (loop (when (null tests)
	  (setq failed-tests (nreverse failed-tests))
	  (if (zerop (length failed-tests))
	      (lisp:format T "~2% XP passed all tests.")
	      (lisp:format T "~2% XP failed ~A tests." (length failed-tests)))
	  (return (values)))
	(lisp:format T " ~A" (car tests))
	(do-test (pop tests))))

(defun more ()
  (if in-tester (throw 'in-tester nil) (do-many-tests)))

(defun do-test (n)
  (catch 'in-tester
    (let* ((info (nth n test-list))
	   (*package* (find-package "USER"))
	   (*break-on-warnings* T)
	   (tester (if (symbolp (car info)) (pop info) 'test-ordinary))
	   (value (cadr info))
	   (pop-if-no-failure nil)
	   (in-tester T))
      (setq form (car info))
      (when (not (member n failed-tests))
	(push n failed-tests)
	(setq pop-if-no-failure T))
      (let ((result (funcall tester (copy-tree form))))
	(when (not (equal result value))
	  (lisp:format t "~%form: ~S~% desired value ~S~%  actual value ~S~%"
		       form value result)
	  (error "failed test"))
	(when pop-if-no-failure
	  (pop failed-tests))
	:ok))))  ;doesn't happen when abort out of error.

;Helper funtions for tests.

(defun test-ordinary (form)
  (setq form `(lambda () ,form))
  (funcall (if compile-tests (compile nil form) form)))

(defun deftest (form) (eval form))

(defun test-def (form)
  (eval (car form))
#+symbolics(compile (cadar form)) ;does not work in all lisps
  (test-ordinary (cadr form)))

(defun etest (form)
  (let ((xp::*testing-errors* T))
     (catch :testing-errors (eval form))))

;This tests things where lisp:format and xp::format must always be identical.

(defmacro formats (f-string &rest args)
  `(if (not (string= (lisp:format nil ,f-string .,args)
		     (xp::format nil (formatter ,f-string) .,args)))
       'format-xp-and-format-disagree))

;this compares FORMAT-XP with FORMAT only on the lispm, so that bugs
;in FORMAT on another system will not trigger XP bug reports
(defmacro format-xps (&rest stuff)
  (let ((format-xp-stuff
	  (mapcar #'(lambda (a) (if (stringp a) `(formatter ,a) a)) stuff)))
  #+symbolics
    `(let* ((format-value (lisp:format nil .,stuff))
	    (xp-value (xp::format nil .,format-xp-stuff)))
       (if (string= format-value xp-value) xp-value
	   `(xp-format-output ,xp-value and
			      lisp-format-output ,format-value disagree)))
  #-symbolics`(xp::format nil .,format-xp-stuff)))

(defmacro plet (width miser &body body)
  `(let ((*PRINT-RIGHT-MARGIN* ,width)
	 (*PRINT-MISER-WIDTH* ,miser)
	 (*PRINT-PRETTY* T)
	 (*PRINT-ARRAY* T)
	 (*PRINT-ESCAPE* T)
	 (*PRINT-CASE* :UPCASE)
	 (*PRINT-CIRCLE* T)
	 (*PRINT-SHARED* nil)
	 (*PRINT-GENSYM* T)
	 (*PRINT-LEVEL* 100)
	 (*PRINT-LENGTH* NIL)
	 (*PRINT-LINES* NIL))
     .,body))

;used to test cases where xp defaults to ordinary non-pretty printing

(defmacro prints (thing &rest bindings)
  `(plet 50 0
     (let* (,@ bindings
	    (xp-result (xp::write-to-string ,thing))
	    (normal-result (let ((*print-pretty* nil)) (lisp:write-to-string ,thing))))
       (if (not (string= xp-result normal-result))
	   `("xp::print-output" ,xp-result and "lisp::print-output"
	     ,normal-result disagree)))))

(defmacro print*s (thing &rest bindings)
  `(plet 50 0
     (let* (,@ bindings
	    (xp-result (xp::write-to-string ,thing))
	    (normal-result (lisp:write-to-string ,thing)))
       (if (string= xp-result normal-result) normal-result
	   `(print*-output ,xp-result and print-output
			   ,normal-result disagree)))))

(defmacro print*c (thing &rest bindings)
  (push '(*print-circle* T) bindings)
  `(plet 150 0
     (let* ,bindings (write-to-string (read-from-string ,thing)))))

(defmacro format*c (string thing &rest bindings)
  (push '(*print-circle* T) bindings)
  `(plet 150 0
     (let* ,bindings
       (xp::format nil ,string (read-from-string ,thing)))))

(defmacro ftest (width miser form &rest bindings)
  `(plet ,width ,miser (let ,bindings (xp::write-to-string ,form))))

(setq test-list '(

;the first bunch of tests test the format directives that xp actually
;calls format to do.  the tests therefore just make sure that xp
;does what format does for these.  if format is wrong in a given lisp,
;xp will be too. 

  ((formats "test ~a." "foo"))
  ((formats "test ~:a." nil))
  ((formats "test ~:a." nil))
  ((formats "test ~21:a." nil))
  ((formats "test ~4:@a." nil))
  ((formats "test ~v:@a." 4 nil))
  ((formats "test ~8,2,3,'-:a." nil))
  ((formats "test ~8,,#,'-:a." nil 1 2))
  ((formats "test ~8,,,'-:a." nil))
  ((formats "test ~8,2,#,v:a." #\- nil 3))
  ((formats "test ~8,2,#,v@a." #\- nil 3))
  ((formats "test ~s." "foo"))
  ((formats "test ~6s." "foo"))
  ((formats "~:d." 12345))
  ((formats "~@d." 12345))
  ((formats "~:b" 22))
  ((formats "~@b" 22))
  ((formats "~:o" 22))
  ((formats "~@o" 22))
  ((formats "~:x" 22))
  ((formats "~@x" 22))
  ((formats "~7:r" 22))
  ((formats "~7@r" 22))
  ((formats "~:r" 4))
  ((formats "~@r" 4))
  ((formats "~:c" #\f))
  ((formats "~@c" #\f))
  ((formats "~6,2f" 3.14159))
  ((formats "~6,2,1,'*f" 100.0))
  ((formats "~9,2,1,,'*e" 3.14159))
  ((formats "~9,2,1,,'*e" 1.1e13))
  ((formats "~9,2,1,,'*g" 0.0314159))
  ((formats "~9,2,1,,'*g" 0.314159))
  ((formats "$~3$" 3.14))
  ((formats "~A-~10<~A~;~A~>-~A" 1 'foo 'bar 2))
  ((formats "~A-~V:<~A~;~A~>-~A" 1 10 'foo 'bar 2))
  ((formats "~A-~10<~(~A~)~;~?~>-~A" 1 'foo "+~A" '(2) 'bar))

;this next set of tests tests format codes which are supposed
;to be supported exactly the same by xp and format, but are actually
;performed directly by xp.  These are compared with the expected
;results, rather than with format so that bugs in format will
;not get reported as bugs in xp.

  ((format-xps "~d tr~:@p/~d win~:p" 7 1) "7 tries/1 win")
  ((format-xps "~d tr~:@p/~d win~:p" 1 0) "1 try/0 wins")
  ((format-xps "~d tr~@p/~d win~p" 1 1 0 0) "1 try/0 wins")

  ((format-xps "test~%  test") "test
  test")
  ((format-xps "test~%~%  test") "test

  test")
  ((format-xps "test~2%  test") "test

  test")
  ((format-xps "test~V%  test" nil) "test
  test")
  ((format-xps "test
  test") "test
  test")
  ((format-xps "test

  test") "test

  test")

  ((format-xps "test~&  test") "test
  test")
  ((format-xps "test~&~&  test") "test
  test")
  ((format-xps "test~2&  test") "test

  test")

  ((format-xps "test ~~ test") "test ~ test")
  ((format-xps "test ~#~ test") "test  test")
  ((format-xps "test ~v~ test" 3) "test ~~~ test")
  ((format-xps "test ~v~ test" nil) "test ~ test")

  ((format-xps "test~
  test") "testtest")
  ((format-xps "test~
  ") "test")
  ((format-xps "test~
 	test") "testtest") ;contains space tab
  ((format-xps "test~:
  test") "test  test")
  ((format-xps "test~@
  test") "test
test")

  ((format-xps "test~ta")        "test a")
  ((format-xps "test~,1ta")      "test a")
  ((format-xps "test2~ta")       "test2 a")
  ((format-xps "-te~5,2ta")      "-te  a")
  ((format-xps "-tes~5,2ta")     "-tes a")
  ((format-xps "-test~5,2ta")    "-test  a")
  ((format-xps "-teste~5,2ta")   "-teste a")
  ((format-xps "-tester~5,2ta")  "-tester  a")
  ((format-xps "-te~5,0ta")      "-te  a")
  ((format-xps "-tes~5,0ta")     "-tes a")
  ((format-xps "-test~5,0ta")    "-testa")
  ((format-xps "-teste~5,0ta")   "-testea")
  ((format-xps "-tester~5,0ta")  "-testera")
  ((format-xps "-te~0,2ta")      "-te a")
  ((format-xps "-tes~0,2ta")     "-tes  a")
  ((format-xps "-test~0,2ta")    "-test a")
  ((format-xps "-teste~0,2ta")   "-teste  a")
  ((format-xps "-tester~0,2ta")  "-tester a")
  ((format-xps "test~8,3ta")     "test    a")
  ((format-xps "test~V,Vta" 3 3) "test  a")

  ((format-xps "+++~%-te~5,2ta")      "+++
-te  a")
  ((format-xps "+++~%-tes~5,2ta")     "+++
-tes a")
  ((format-xps "+++~%-test~5,2ta")    "+++
-test  a")
  ((format-xps "+++~%-teste~5,2ta")   "+++
-teste a")
  ((format-xps "+++~%-tester~5,2ta")  "+++
-tester  a")

  ((format-xps "test~@ta") "test a")
  ((format-xps "-te~1,2@ta")      "-te a")
  ((format-xps "-tes~1,2@ta")     "-tes  a")
  ((format-xps "-test~1,2@ta")    "-test a")
  ((format-xps "-teste~1,2@ta")   "-teste  a")
  ((format-xps "-tester~1,2@ta")  "-tester a")
  ((format-xps "-te~0,2@ta")      "-te a")
  ((format-xps "-tes~0,2@ta")     "-tesa")
  ((format-xps "-test~0,2@ta")    "-test a")
  ((format-xps "-teste~0,2@ta")   "-testea")
  ((format-xps "-tester~0,2@ta")  "-tester a")
  ((format-xps "-te~3,0@ta")      "-te   a")
  ((format-xps "-tes~3,0@ta")     "-tes   a")
  ((format-xps "-test~3,0@ta")    "-test   a")
  ((format-xps "-te~3@ta")        "-te   a")
  ((format-xps "-tes~3@ta")       "-tes   a")
  ((format-xps "-test~3@ta")      "-test   a")
  ((format-xps "-te~0,0@ta")      "-tea")
  ((format-xps "-tes~0,0@ta")     "-tesa")
  ((format-xps "-test~0,0@ta")    "-testa")
  ((format-xps "test~8@ta")       "test        a")
  ((format-xps "test~8,3@ta")     "test        a")
  ((format-xps "test~V,V@ta" 8 5) "test           a")

  ((format-xps "~a~a~*~a" 1 2 3 4 5 6 7) "124")
  ((format-xps "~a~a~2*~a" 1 2 3 4 5 6 7) "125")
  ((format-xps "~a~a~V*~a" 1 2 3 4 5 6 7) "127")
  ((format-xps "~a~a~:*~a" 1 2 3 4 5 6 7) "122")
  ((format-xps "~a~a~2:*~a" 1 2 3 4 5 6 7) "121")
  ((format-xps "~a~a~V:*~a" 1 2 3 4 5 6 7) "121")
  ((format-xps "~a~a~1@*~a" 1 2 3 4 5 6 7) "122")
  ((format-xps "~a~a~3@*~a" 1 2 3 4 5 6 7) "124")
  ((format-xps "~a~a~V@*~a" 1 2 3 4 5 6 7) "124")

  ;; ~* in various subcontexts tested below.

  ((format-xps "test~d ~? test" 2 "(item ~a)" '(4))
   "test2 (item 4) test")
  ((xp::format nil (formatter "test~d ~? test") 2 "(item ~a)" '(4))
   "test2 (item 4) test")
  ((let ((string "(item ~a)"))
     (list (xp::format nil string 4)
	   (xp::format nil (formatter "test~d ~@? test") 2 string 4)
	   (xp::format nil (formatter "test~d ~@? test") 2 string 4)))
   ("(item 4)" "test2 (item 4) test" "test2 (item 4) test"))
  ((plet 20 0 (xp::format nil (formatter "test~d ~? test") 2 (formatter "(item ~a~^)") '(4)))
   "test2 (item 4 test")
  ((plet 20 0 (xp::format nil (formatter "test~d ~? ~D test")
			  2 (formatter "(item ~a~0^)") '(4 5) 6))
   "test2 (item 4 6 test")

  ((format-xps "test~d ~@? test ~A" 2 "(item ~a)" 4 5)
   "test2 (item 4) test 5")
  ((xp::format nil (formatter "test~d ~@? test ~A") 2 "(item ~a)" 4 5)
   "test2 (item 4) test 5")
  ((xp::format nil (formatter "test~d ~@? test ~A") 2 "(item ~a~1:*)" 4 5)
   "test2 (item 4) test 4")
  ((plet 20 0 (xp::format nil (formatter "test~d ~@? test ~A")
			  2 (formatter "(item ~a~0^)") 4 5))
   "test2 (item 4 test 5")

  ((format-xps "tEst~(tesT ~S~) test" 'one)
   "tEsttest one test")
  ((format-xps "tEst~:(tesT ~S~) test" 'one)
   "tEstTest One test")
  ((format-xps "tEst~:(tesT~S~) test" 'one)
   "tEstTestone test")
  ((xp::format nil (formatter "tEst~:( tesT~T~S~) test") 'one)
   "tEst Test One test")
  ((format-xps "tEst~@( tesT ~S~) test" 'one)
   "tEst Test one test")
  ((format-xps "tEst~:@( tesT ~S~) test" 'one)
   "tEst TEST ONE test")
  ((plet 44 0 (xp::format nil (formatter "~:(~W~)")
		       '(compiler-let ((a (foo 3)) (b (foo 4)) (c 1))
			  (tuz a b))))
   "(Compiler-Let ((A (Foo 3))
               (B (Foo 4))
               (C 1))
  (Tuz A B))")
  ((plet 50 0 (xp::format nil
     (formatter "foo ~@<aa~@;p~:@_ ~:@(hi ~@<bb ~@;q~(~:@_R~:@_S~)~:>~:@_t~)~:>u")))
   "foo aap
    aa HI BB Q
    aa    BB R
    aa    BB S
    aaTu")

  ((format-xps "~[a~;b~;c~]" 1) "b")
  ((format-xps "~2[a~;b~;c~]") "c")
  ((format-xps "~[foo~]" 1) "")
  ((format-xps "~[a~;b~:;c~]" 10) "c")
  ((format-xps "~:[a~;b~]" nil) "a")
  ((format-xps "~:[a~;b~]" 3) "b")
  ((format-xps "~@[~A~A~] ~A" 1 2 3) "12 3")
  ((format-xps "~@[~A~A~] ~A" nil 2 3) " 2")

  ((format-xps "~{~a~^,~}." '(1 2 3 4)) "1,2,3,4.")
  ((format-xps "~V{~a~^,~}." 2 '(1 2 3 4)) "1,2,.")  
  ((format-xps "~2{~a~^,~}." '(1)) "1.")
  ((format-xps "~{foo~:}." '()) "foo.")
  ((format-xps "~{~a~#,1^,~}." '(1 2 3 4)) "1,2,3.")
  ((format-xps "~{~a~3@*~^,~a~^,~}." '(1 2 3 4 5 6 7 8 9)) "1,4,5,8,9.")

  ((format-xps "~:{~a~^,~}." '((1 2) (3 4))) "1,3,.")
  ((format-xps "~V:{~a~^,~}." 1 '((1 2) (3 4))) "1,.")
  ((format-xps "~1:{~a~^,~}."  '()) ".")
  ((format-xps "~:{foo~:}."  '()) "foo.")
  ((format-xps "~:{~a~:^,~}." '((1 2) (3 4))) "1,3.")
  ((format-xps "~:{~a~1,1:^,~}." '((1 2) (3 4))) "1.")
  ((format-xps "~:{~a~#,1:^,~}." '((1 2) (3 4))) "1.")
  ((format-xps "~:{~a~#:^,~}." '((1) (3 4))) "1.")
  ((format-xps "~:{~a~3@*~^,~a~^,~}." '((1 2 3 4 5) (6 7 8 9))) "1,4,6,9.")

  ((format-xps "~@{~a~^,~}." 1 2 3 4) "1,2,3,4.")
  ((format-xps "~@{~a~1,1^,~}." 1 2 3 4) "1.")
  ((format-xps "~@{~a~1,1,1^,~}." 1 2 3 4) "1.")
  ((format-xps "~@{~a~2,1,1^,~}." 1 2 3 4) "1,2,3,4,.")
  ((format-xps "~V@{~a~^,~}." 2 1 2 3 4) "1,2,.")
  ((format-xps "~2@{~a~^,~}." 1) "1.")
  ((format-xps "~@{foo~:}.") "foo.")
  ((plet 20 0 (xp::format nil (formatter "~@{~a~#,1^,~} ~A.") 1 2 3 4)) "1,2,3 4.")
  ((format-xps "~@{~a~3@*~^,~a~^,~}." 1 2 3 4 5 6 7 8 9) "1,4,5,8,9.")

  ((format-xps "~:@{~a~^,~}." '(1 2) '(3 4)) "1,3,.")
  ((format-xps "~V:@{~a~^,~}." 1 '(1 2) '(3 4)) "1,.")
  ((format-xps "~1:@{~a~^,~}.") ".")
  ((format-xps "~:@{foo~:}.") "foo.")
  ((format-xps "~:@{foo~}.") ".")
  ((format-xps "~:@{~a~:^,~}." '(1 2) '(3 4)) "1,3.")
  ((format-xps "~:@{~a~1,1:^,~}." '(1 2) '(3 4)) "1.")
  ((format-xps "~:@{~a~#,1:^,~}." '(1 2) '(3 4)) "1.")
  ((format-xps "~:@{~a~#:^,~}." '(1) '(3 4)) "1.")
  ((format-xps "~:@{~a~3@*~^,~a~^,~}." '(1 2 3 4 5) '(6 7 8 9)) "1,4,6,9.")

  ((format-xps "~{~}." "~A+~A," '(1 2 3 4)) "1+2,3+4,.")
  ((format-xps "~@{~}." "~A+~A," 1 2 3 4) "1+2,3+4,.")
  ((format-xps "~:{~}." "~A+~A," '((1 2) (3 4))) "1+2,3+4,.")
  ((format-xps "~:@{~}." "~A+~A," '(1 2) '(3 4)) "1+2,3+4,.")
  ((xp::format nil (formatter "~{~}.") (formatter "~A~^+~A,") '(1 2 3)) "1+2,3.")
  ((xp::format nil (formatter "~:{~}.") (formatter "~A~^+~A,") '((1) (2 3))) "12+3,.")

;; ~^ tested in the relevant subcontexts above and below


;the following test extended features of standard format codes.

  ((xp::format nil (formatter "test~:ta")) "test a")
  ((xp::format nil (formatter "test~8:ta")) "test    a")
  ((xp::format nil (formatter "test~V,V:ta") 8 3) "test    a")
  ((xp::format nil (formatter "test~0,3:ta")) "test  a")
  ((xp::format nil (formatter "test~0,4:ta")) "test    a")
  ((xp::format nil (formatter "test~0,5:ta")) "test a")

  ((xp::format nil (formatter "test~@:ta")) "test a")
  ((xp::format nil (formatter "test~8@:ta")) "test        a")
  ((xp::format nil (formatter "test~V,V@:ta") 8 3) "test        a")
  ((xp::format nil (formatter "test~8,5@:ta")) "test           a")
  ((xp::format nil (formatter "test~0,3@:ta")) "test  a")
  ((xp::format nil (formatter "test~0,4@:ta")) "testa")

  ((xp::format nil (formatter "fo-~<test~:ta~:>")) "fo-test a")
  ((xp::format nil (formatter "fo-~<test~8:ta~:>")) "fo-test    a")
  ((xp::format nil (formatter "fo-~<test~8,3:ta~:>")) "fo-test    a")
  ((xp::format nil (formatter "fo-~<test~0,3:ta~:>")) "fo-test  a")
  ((xp::format nil (formatter "fo-~<test~0,4:ta~:>")) "fo-test    a")
  ((xp::format nil (formatter "fo-~<test~0,5:ta~:>")) "fo-test a")

  ((xp::format nil (formatter "fo-~:@_~<test~0,3ta~:>")) "fo-
test  a")
  ((xp::format nil (formatter "fo-~:@_~<test~0,4ta~:>")) "fo-
test    a")
  ((xp::format nil (formatter "fo-~:@_~<test~0,5ta~:>")) "fo-
test a")

  ((xp::format nil (formatter "fo-~<test~:@ta~:>")) "fo-test a")
  ((xp::format nil (formatter "fo-~<test~8:@ta~:>")) "fo-test        a")
  ((xp::format nil (formatter "fo-~<test~8,3:@ta~:>")) "fo-test        a")
  ((xp::format nil (formatter "fo-~<test~8,5:@ta~:>")) "fo-test           a")
  ((xp::format nil (formatter "fo-~<test~0,3:@ta~:>")) "fo-test  a")
  ((xp::format nil (formatter "fo-~<test~0,4:@ta~:>")) "fo-testa")

  ((xp::format nil (formatter "fo-~<test~6,4ta~:>")) "fo-test   a")
  ((xp::format nil (formatter "fo-~<test~6,3ta~:>")) "fo-test  a")

;The following test the special pretty printing directives.

  ((plet 20 0 (xp::format nil (formatter "--~@<~a~a~:>-~a") 1 2 3 4 5)) "--12-NIL")
  ((plet 20 0 (xp::format nil (formatter "~<--~@<~a~a~:>-~a~:>") '(1 2 3 4 5))) "--12-NIL")
  ((plet 20 0 (xp::format nil (formatter "~<~@{~A~^ ~}~:>") '(1 2 . 3))) "1 2 . 3")

  ((plet 20 0 (xp::format nil (formatter "tes~<t~ta~:>"))) "test a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~8ta~:>"))) "test    a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~8,3ta~:>"))) "test    a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~0,3ta~:>"))) "test  a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~0,4ta~:>"))) "test    a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~0,5ta~:>"))) "test a")

  ((plet 20 0 (xp::format nil (formatter "tes~<t~@ta~:>"))) "test a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~8@ta~:>"))) "test        a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~8,3@ta~:>"))) "test        a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~8,5@ta~:>"))) "test           a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~0,3@ta~:>"))) "test  a")
  ((plet 20 0 (xp::format nil (formatter "tes~<t~0,4@ta~:>"))) "testa")

  ((plet 20 0 (xp::format nil (formatter "~a~<~a~*~a~:>~a") 1 '(2 3 4 5 6 7) 0)) "1240")
  ((plet 20 0 (xp::format nil (formatter "~a~<~a~0@*~a~:>~a") 1 '(2 3 4 5 6 7) 0)) "1220")
  ((plet 20 0 (xp::format nil (formatter "~a~@<~a~*~a~:>") 1 2 3 4 5 6 7)) "124")
  ((plet 20 0 (xp::format nil (formatter "~a~@<~a~0@*~a~:>") 1 2 3 4 5 6 7)) "122")

  ((plet 20 0 (xp::format nil (formatter "~a~<~a~^~a~:>~a") 1 '(2) 0)) "120")
  ((plet 20 0 (xp::format nil (formatter "~a~<~a~^~a~:>~a") 1 '(2) 0)) "120")
  ((plet 20 0 (xp::format nil (formatter "~a~@<~a~#,4^~a~:>") 1 2 3 4 5 6)) "12")
  ((plet 16 0
     (let ((*print-length* 2))
       (xp::format nil (formatter "---~<~a~^ ~a~^ ~a~:>--") '(12 3456 789))))
   "---12 3456 ...--")
  ((plet 16 0
     (let ((*print-length* 1))
       (xp::format nil (formatter "---~<~a~^ ~a~^ ~a~:>--") '(12 3456 789))))
   "---12 ...--")
  ((plet 16 0
     (with-output-to-string (s)
       (let ((*print-length* 1))
	 (xp::format s (formatter "---~<~a~^ ~a~^ ~a~:>--") '(12 3456 789)))
       (princ " " s)
       (funcall *last-abbreviated-printing*)))
   "---12 ...-- ---12 3456 789--")

  ((plet 16 0
     (let ((*print-length* 2))
       (xp::format nil (formatter "---~@<~a~^ ~a~^ ~a~:>--") 12 3456 789)))
   "---12 3456 ...--")
  ((plet 16 0
     (let ((*print-length* 2))
       (xp::format nil (formatter "---~:@<~a~^ ~a~^ ~a~:>--") 12 3456 789)))
   "---(12 3456 ...)--")
  ((plet 16 0
     (let ((*print-length* 2))
       (xp::format nil (formatter "---~:@<~a~^ ~a~^ ~a~:>~A--") 12 3456 789)))
   "---(12 3456 ...)NIL--")

  ((plet 20 0 (xp::format nil (formatter "test~<a~2%  test~:>b"))) "testa

  testb")
  ((plet 20 0 (xp::format nil (formatter "test~<a
  test~:>b"))) "testa
  testb")

  ((plet 20 0 (xp::format nil (formatter "test~<a~&  test~:>b"))) "testa
  testb")

  ((plet 20 0 (xp::format nil (formatter "~a~:@<~:>") 1 2 4 5)) "1()")
  ((plet 20 0 (xp::format nil (formatter "~a~:@<~a~a~:>") 1 2 4 5)) "1(24)")
  ((plet 20 0 (xp::format nil (formatter "~a~@<+~;~a~a~:>") 1 2 4 5)) "1+24")
  ((plet 20 0 (xp::format nil (formatter "~a~:@<+~;~a~a~:>") 1 2 4 5)) "1+24)")
  ((plet 20 0 (xp::format nil (formatter "~a~@<+(~;~a~a~;)*~:>") 1 2 4 5)) "1+(24)*")
  ((plet 20 0 (xp::format nil (formatter "~a~:@<+~;~a~a~;*~:>") 1 2 4 5)) "1+24*")
   
  ((plet 50 0
     (xp::format nil (formatter "foo ~@<++~@;1~:@_2~:@_3~:>4")))
   "foo ++1
    ++2
    ++34")
  ((plet 50 0
     (xp::format nil (formatter "foo ~@<++~@;1~:@_ hi ~@<##~@;1~:@_2~:@_3~:>~:@_3~:>4")))
   "foo ++1
    ++ hi ##1
    ++    ##2
    ++    ##3
    ++34")
  ((plet 50 0
     (xp::format nil (formatter "foo ~@<++~@;1~:@_2 ~S~:@_3~:>4") "testing
linebreaks"))
"foo ++1
    ++2 \"testing
    ++linebreaks\"
    ++34")

  ((plet 18 0 (xp::format nil (formatter "---~:<~a ~_~a ~_~a~:>--") '(12 3456 789)))
   "---(12 3456 789)--")
  ((plet 17 0 (xp::format nil (formatter "---~:<~a ~_~a ~_~a~:>--") '(12 3456 789)))
   "---(12
    3456
    789)--")
  ((plet 13 0 (xp::format nil (formatter "---~{~a ~_~a~2I ~_~a~}--") '(12 3456 789)))
   "---12
3456
  789--")
  ((plet 17 0 (xp::format nil (formatter "---~<<~;~a ~_~a ~_~a~;>~:>--") '(12 3456 789)))
   "---<12
    3456
    789>--")
  ((plet 17 0 (xp::format nil (formatter "---~<<~@;~a ~_~a ~_~a~;>~:>--") '(12 3456 789)))
   "---<12
   <3456
   <789>--")
  ((plet 16 0 (xp::format nil (formatter "---~<<~@;~a ~_~a ~_~a~:>--") '(12 3456 789)))
   "---<12
   <3456
   <789--")

  ((plet 15 0
     (xp::format nil (formatter "---~<12	3456 789~:@>--"))) ;note tab after "12"
   "---12	3456
   789--")
  ((plet 15 0 (xp::format nil (formatter "---~<~a ~a ~a~:@>--") '(12 3456 789)))
   "---12 3456
   789--")
  ((plet 15 0 (xp::format nil (formatter "---~<~a ~@{~a ~a~}~:@>--") '(12 3456 789)))
   "---12
   3456 789--")
  ((plet 25 0 (xp::format nil (formatter "---~<~a	~a-~a~@:>--") '(12 3456 789))) ;note tab char
   "---12	3456-789--")

  ((plet 15 0
     (let ((*print-level* 3))
       (xp::format nil (formatter "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
   "0(1(2(3)))")
  ((plet 15 0
     (let ((*print-level* 2))
       (xp::format nil (formatter "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
   "0(1(2#))")
  ((plet 15 0
     (let ((*print-level* 1))
       (xp::format nil (formatter "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
   "0(1#)")  
  ((plet 15 0
     (let ((*print-level* 0))
       (xp::format nil (formatter "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
   "0#")
  ((plet 15 0
     (with-output-to-string (s)
       (let ((*print-level* 1))
	 (xp::format s (formatter "0~:@<1~:@<2~:@<3~:>~:>~:>")))
       (xp::format s " ")
       (xp::format s (formatter "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
   "0(1#) 0(1(2(3)))")
  ((plet 50 0
     (let ((*print-level* 1))
       (xp::format nil (formatter "~:<~W~:@<~W~:>~:>") '(0 1 2 3 4))))
   "(0#)")

  ((plet 16 0 (xp::format nil (formatter "---~<~a ~_~a ~_~a~:>--") '(12 3456 789)))
   "---12 3456 789--")
  ((plet 15 0 (xp::format nil (formatter "---~<~a ~_~a ~_~a~:>--") '(12 3456 789)))
   "---12
   3456
   789--")

  ((plet 16 0 (xp::format nil (formatter "---~<~a ~:_~a ~:_~a~:>--") '(12 3456 789)))
   "---12 3456 789--")
  ((plet 11 0 (xp::format nil (formatter "---~<~a ~:_~a ~:_~a~:>--") '(12 3456 789)))
   "---12 3456
   789--")
  ((plet 10 0 (xp::format nil (formatter "---~<~a ~:_~a ~:_~a~:>--") '(12 3456 789)))
   "---12
   3456
   789--")
  ((plet 50 0 (xp::format nil (formatter "---~<~a ~<<<~:@_>>~:>~:_~a~:>--") '(12 () 789)))
   "---12 <<
      >>
   789--")
  ((plet 50 0 (xp::format nil (formatter "---~<~a ~:_~<<<~:@_>>~:>~:_~a~:>--")
		       '(12 () 789)))
   "---12
   <<
   >>
   789--")

  ((plet 16 0 (xp::format nil (formatter "---~<~a ~@_~a ~:_~a~:>--") '(12 3456 789)))
   "---12 3456 789--")
  ((plet 11 0 (xp::format nil (formatter "---~<~a ~@_~a ~:_~a~:>--") '(12 3456 789)))
   "---12 3456
   789--")
  ((plet 11 nil (xp::format nil (formatter "---~<~a ~@_~a ~:_~a~:>--") '(12 3456 789)))
   "---12 3456
   789--")
  ((plet 11 20 (xp::format nil (formatter "---~<~a ~@_~a ~:_~a~:>--") '(12 3456 789)))
   "---12
   3456
   789--")

  ((plet 25 0 (xp::format nil (formatter "---~<~a ~:@_~a ~_~a~:>--") '(12 3456 789)))
   "---12
   3456
   789--")
  ((plet 13 0 (xp::format nil (formatter "---~<~a ~:@_~a ~:_~a~:>--") '(12 3456 789)))
   "---12
   3456 789--")
  ((plet 12 0 (xp::format nil (formatter "---~<~a ~:@_~a ~:_~a~:>--") '(12 3456 789)))
   "---12
   3456
   789--")

  ((plet 15 0 (xp::format nil (formatter "---~<~a~1I ~_~a ~_~a~:>--") '(12 3456 789)))
   "---12
    3456
    789--")
 ((plet 15 0 (xp::format nil (formatter "---~<~a~-2I ~_~a ~_~a~:>--") '(12 3456 789)))
   "---12
 3456
 789--")
 ((plet 15 0 (xp::format nil (formatter "---~<~a~VI ~_~a ~_~a~:>--") '(12 -2 3456 789)))
   "---12
 3456
 789--")
  ((plet 15 0 (xp::format nil (formatter "---~<~a~:I ~_~a ~_~a~:>--") '(12 3456 789)))
   "---12
     3456
     789--")
  ((plet 15 20 (xp::format nil (formatter "---~<~a~:I ~_~a ~_~a~:>--") '(12 3456 789)))
   "---12
   3456
   789--")
  ((plet 15 0 (xp::format nil (formatter "---~<~a ~_~a~-1:I ~_~a~:>--") '(12 3456 789)))
   "---12
   3456
      789--")
  ((plet 15 0 (xp::format nil (formatter "---~<~a ~_~a~V:I ~_~a~:>--") '(12 3456 -1 789)))
   "---12
   3456
      789--")

  ((plet 16 0
     (let ((*print-length* 3))
       (xp::format nil (formatter "---~<~a ~_~a ~_~a~:>--") '(12 3456 789))))
   "---12 3456 789--")

  ((plet 15 0
     (let ((*print-lines* 3))
       (xp::format nil (formatter "---~<~a ~_~a ~_~a~:>--") '(12 3456 789))))
   "---12
   3456
   789--")
  ((plet 15 0
     (let ((*print-lines* 2))
       (xp::format nil (formatter "---~<~a ~_~a ~_~a~:>--") '(12 3456 789))))
   "---12
   3456 ..")
  ((plet 15 0
     (let ((*print-lines* 1))
       (xp::format nil (formatter "---~<~a ~_~a ~_~a~:>--") '(12 3456 789))))
   "---12 ..")
  ((plet 15 0
     (with-output-to-string (s)
       (let ((*print-lines* 1))
	 (xp::format s (formatter "---~:<~a ~_~a ~_~a~:>--") '(12 3456 789)))
       (terpri s)
       (funcall *last-abbreviated-printing*)))
   "---(12 ..)
---(12
    3456
    789)--")

  ((plet 15 0 (xp::format nil (formatter "---~/pprint-fill/--") '(12 3456 789)))
   "---12 3456
   789--")
  ((plet 15 0 (xp::format nil (formatter "---~:/pprint-fill/--") '(12 3456 789)))
   "---(12 3456
    789)--")
  ((plet 15 0 (xp::format nil (formatter "---~:/pprint-fill/--") '12))
   "---12--")
  ((plet 15 0 (let ((*print-level* 4) (*print-length* 2))
		(xp::format nil (formatter "---~:/pprint-fill/--") '(12 3456 789))))
   "---(12 3456
    ...)--")
  ((plet 25 0 (let ((*print-level* 4) (*print-length* 2))
		(xp::format nil (formatter "---~/pprint-fill/--") '(12 3456 789))))
   "---12 3456 ...--")
  ((plet 15 0 (let ((*print-level* 0))
		(xp::format nil (formatter "---~:/pprint-fill/--") '(12 3456 789))))
   "---#--")
  ((plet 15 0 (let ((*print-level* 0))
		(xp::format nil (formatter "---~/pprint-fill/--") '(12 3456 789))))
   "---#--")

  ((plet 15 0 (xp::format nil (formatter "---~/pprint-linear/--") '(12 3456 789)))
   "---12
   3456
   789--")
  ((plet 15 0 (xp::format nil (formatter "---~:/pprint-linear/--") '(12 3456 789)))
   "---(12
    3456
    789)--")
  ((plet 15 0 (xp::format nil (formatter "---~:/pprint-linear/--") '12))
   "---12--")
  ((plet 15 0 (let ((*print-level* 4) (*print-length* 2))
		(xp::format nil (formatter "---~:/pprint-linear/--") '(12 3456 789))))
   "---(12
    3456
    ...)--")
  ((plet 25 0 (let ((*print-level* 4) (*print-length* 2))
		(xp::format nil (formatter "---~/pprint-linear/--") '(12 3456 789))))
   "---12 3456 ...--")
  ((plet 15 0 (let ((*print-level* 0))
		(xp::format nil (formatter "---~:/pprint-linear/--") '(12 3456 789))))
   "---#--")
  ((plet 15 0 (let ((*print-level* 0))
		(xp::format nil (formatter "---~/pprint-linear/--") '(12 3456 789))))
   "---#--")

  ((plet 100 0 (xp::format nil (formatter "---~5/pprint-tabular/--")
		       '(12 3456 789 22 45656 78)))
   "---12   3456 789  22   45656     78--")
  ((plet 100 0 (xp::format nil (formatter "---~5/pprint-tabular/--")
		       '(12+++ 3456 789 22 45656 78)))
   "---12+++     3456 789  22   45656     78--")
  ((plet 100 0
     (let ((*print-length* 3))
       (xp::format nil (formatter "---~5/pprint-tabular/--")
		  '(12 3456 789 22 456 78))))
   "---12   3456 789  ...--")
  ((plet 21 0 (xp::format nil (formatter "---~5/pprint-tabular/--")
		       '(12 3456 789 22 456 78)))
   "---12   3456 789
   22   456  78--")
  ((plet 21 0 (xp::format nil (formatter "---~5:/pprint-tabular/--")
		       '(12 3456 789 22 456 78)))
   "---(12   3456 789
    22   456  78)--")
  ((plet 100 0
     (let ((*print-length* 3))
       (xp::format nil (formatter "---~5:/pprint-tabular/--")
		  '(12 3456 789 22 456 78))))
   "---(12   3456 789  ...)--")
  ((plet 41 0 (xp::format nil (formatter "---~V/pprint-tabular/--")
		       nil '(12 3456 789 22 456 78)))
   "---12              3456
   789             22
   456             78--")
  ((plet 21 0 (xp::format nil (formatter "---~5:/pprint-tabular/--") ()))
   "---()--")
  ((plet 21 0 (xp::format nil (formatter "---~5:/pprint-tabular/--") 12))
   "---12--")
  ((plet 21 0 (let ((*print-level* 0)) (xp::format nil (formatter "---~5/pprint-tabular/--") ())))
   "---#--")
  ((plet 21 0 (let ((*print-level* 0)) (xp::format nil (formatter "---~5:/pprint-tabular/--") ())))
   "---#--")

  ((plet 90 0
     (let ((*print-escape* nil))
       (xp::format nil (formatter "~W") "foo")))
   "foo")
  ((plet 90 0
     (let ((*print-escape* T))
       (xp::format nil (formatter "~W") "foo")))
   "\"foo\"")

  ((plet 10 0
     (let ((*print-pretty* nil))
       (prints '#(12 3456 789 22 456 78)))))
  ((plet 15 0
     (let ((*print-length* 5))
       (xp::format nil (formatter "~W") '#(12 3456 789 22 456 78))))
   "#(12 3456 789
  22 456 ...)")
  ((plet 15 0
     (let ((*print-length* 0))
       (xp::format nil (formatter "~W") '#(12 3456 789 22 456 78))))
   "#(...)")
  ((plet 15 0
     (let ((*print-level* 0))
       (xp::format nil (formatter "~W") '#(12 3456 789 22 456 78))))
   "#")
  ((plet 10 0
     (let ((*print-pretty* nil)) (prints '#()))))

  ((plet 10 0
     (let ((*print-pretty* nil))
       (string-upcase
	 (xp::format nil (formatter "~W") '#2A((12 3456 789) (22 456 78))))))
   "#2A((12 3456 789) (22 456 78))")
  ((plet 20 0
     (let ((*print-pretty* T))
       (string-upcase
	 (xp::format nil (formatter "~W") '#2A((12 3456 789) (22 456 78))))))
   "#2A((12 3456 789)
    (22 456 78))")
  ((plet 17 0
     (let ((*print-pretty* T))
       (string-upcase
	 (xp::format nil (formatter "~W") '#2A((12 3456 789) (22 456 78))))))
   "#2A((12 3456
     789)
    (22 456 78))")
  ((plet 10 0
     (let ((*print-pretty* T))
       (xp::format nil (formatter "~W") '#0Afoo)))
   "#0A FOO")
  ((plet 30 0
     (let ((*print-pretty* T))
       (string-upcase
	 (xp::format nil (formatter "~W") '#3A(((1 12) (1 3456) (1 789))
					       ((1 22) (1 456) (1 78)))))))
   "#3A(((1 12) (1 3456) (1 789))
    ((1 22) (1 456) (1 78)))")
  ((plet 30 0
     (let ((*print-pretty* T))
       (string-upcase
	 (xp::format nil (formatter "~W") '#3A((() ()) (() ()))))))
   "#3A((() ()) (() ()))")
  ((plet 30 0
     (let ((*print-pretty* T) (*print-level* 2))
       (string-upcase
	 (xp::format nil (formatter "~W") '#3A(((1 12) (1 3456) (1 789))
					       ((1 22) (1 456) (1 78)))))))
   "#3A((# # #) (# # #))")
  ((plet 30 0
     (let ((*print-pretty* T) (*print-level* 0))
       (string-upcase
	 (xp::format nil (formatter "~W") '#3A(((1 12) (1 3456) (1 789))
					       ((1 22) (1 456) (1 78)))))))
   "#3A#")
  ((plet 30 0
     (let ((*print-pretty* T) (*print-length* 4))
       (string-upcase
	 (xp::format nil (formatter "~W") '#3A(((1 12 1 1 1 1) (1 3456 1 1 1 1))
					       ((1 22 1 1 1 1) (1 456 1 1 1 1)))))))
     "#3A(((1 12 1 1 ...)
     (1 3456 1 1 ...))
    ((1 22 1 1 ...)
     (1 456 1 1 ...)))")
  ((plet 30 0
     (let ((*print-pretty* T) (*print-length* 0))
       (string-upcase
	 (xp::format nil (formatter "~W") '#3A(((1 12 1 1 1 1) (1 3456 1 1 1 1))
					       ((1 22 1 1 1 1) (1 456 1 1 1 1)))))))
     "#3A(...)")

  (test-def ((defun foo (xp obj colon atsign &optional (n 3))
	       (if colon (push ":" obj))
	       (if atsign (push "@" obj))
	       (xp::format xp (formatter "~V{~A~}") n obj))
	     (flet ((foo (xp obj colon atsign &optional (n 3))
		      (declare (ignore colon atsign n))
		      (xp::format xp (formatter "~A") obj)))
	       (plet 20 0
		 (list (with-output-to-string (s)
			 (foo s '(1 2 3 4) nil nil))
		       (xp::format nil (formatter "-~/foo/-") '(1 2 3 4))))))
	    ("(1 2 3 4)" "-123-"))
  (test-def ((defun foo1 (xp obj colon atsign &optional (n 3))
	       (if colon (push ":" obj))
	       (if atsign (push "@" obj))
	       (xp::format xp (formatter "~V{~A~}") n obj))
	     (plet 20 0
	       (xp::format nil (formatter "-~4/foo1/-") '(1 2 3 4)))) "-1234-")
  (test-def ((defun foo2 (xp obj colon atsign &optional (n 3))
	       (if colon (push ":" obj))
	       (if atsign (push "@" obj))
	       (xp::format xp (formatter "~V{~A~}") n obj))
	     (plet 20 0
	       (xp::format nil (formatter "-~#/foo2/-") '(1 2 3 4)))) "-1-")
  (test-def ((defun foo3 (xp obj colon atsign &optional (n 3))
	       (if colon (push ":" obj))
	       (if atsign (push "@" obj))
	       (xp::format xp (formatter "~V{~A~}") n obj))
	     (plet 20 0
	       (xp::format nil (formatter "-~V/foo3/-") 2 '(1 2 3 4)))) "-12-")
  (test-def ((defun foo4 (xp obj colon atsign &optional (n 3))
	       (if colon (push ":" obj))
	       (if atsign (push "@" obj))
	       (xp::format xp (formatter "~V{~A~}") n obj))
	     (plet 20 0
	       (xp::format nil (formatter "-~:@/foo4/-") '(1 2 3 4)))) "-@:1-")
  (test-def ((defun foo5 (xp obj colon atsign &optional (n 3))
	       (if colon (push ":" obj))
	       (if atsign (push "@" obj))
	       (xp::format xp (formatter "~V{~A~}") n obj))
	     (plet 20 0
	       (let ((*package* (find-package "XP")))
		 (xp::format nil (formatter "-~/foo5/-") '(1 2 3 4))))) "-123-")
  (test-def ((defun foo6 (xp obj colon atsign &optional (n 3))
	       (if colon (push ":" obj))
	       (if atsign (push "@" obj))
	       (xp::format xp (formatter "~V{~A~}") n obj))
	     (plet 20 0
	       (let ((*package* (find-package "XP")))
		 (xp::format nil (formatter "-~/user::foo6/-") '(1 2 3 4))))) "-123-")
  (test-def ((defun bar (xp &rest objects)
	       (xp::format xp (formatter "~{~A~}") objects))
	     (plet 20 0
	       (xp::format nil (formatter "-~?-") #'bar '(1 2 3 4)))) "-1234-")

;tests of xp's special printing functions.

  ((let ((s (make-array 20 :element-type 'string-char :fill-pointer 0)))
     (list (xp::format s (formatter "test~A ") "ing")
	   (xp::format s (formatter "test~A ") "ing")
	   s))
   (nil nil "testing testing "))
  ((plet 10 0
     (let ((*print-pretty* nil) x)
      (list
       (with-output-to-string (s)
	 (setq x (xp::write '(setq a 8 c "2") :stream s :base 8 :lines 4)))
       x)))
   ("(SETQ A 10 C \"2\")" (setq a 8 c "2")))
  ((plet 10 0
     (let ((*print-pretty* nil))
       (with-output-to-string (s)
	 (xp::write '(setq a 8 c "2") :stream s :escape nil :pretty T))))
   "(SETQ A 8
      C 2)")
  (test-def ((defun bar1 (xp list &rest stuff) (declare (ignore stuff))
		    (xp::write list :stream xp :length 4 :pretty nil))
	     (plet 10 0
	       (xp::format nil (formatter "-~/bar1/-") '(setq a 8 c "2"))))
	    "-(SETQ A 8 C ...)-")
  (test-def ((defun bar2 (xp list &rest stuff) (declare (ignore stuff))
		    (xp::write list :stream xp :length 4))
	     (plet 14 0
	       (xp::format nil (formatter "-~/bar2/-") '(setq a 8 c "2"))))
	    "-(SETQ A 8
       C ...)-")

  ((plet 10 0
     (let ((*print-pretty* nil) x)
      (cons
       (with-output-to-string (s)
	(setq x (list
	 (xp::prin1 "2" s)
	 (xp::fresh-line s)
	 (xp::write-line "This is a test" s :start 2)
	 (xp::terpri s)
	 (xp::write-string "This is a test" s :end 7)
	 (multiple-value-list (xp::pprint '(setq a b c d) s)) 
         (xp::write-string "more
tests" s)
	 (xp::print "2" s)
	 (xp::write-char #\a s)
	 (xp::write-char #\newline s)
	 (xp::fresh-line s)
	 (xp::princ "2" s))))
       x)))
   ("\"2\"
is is a test

This is
\(SETQ A B
      C D)more
tests
\"2\" a
2" 
    "2" T "This is a test" nil "This is a test" nil "more
tests" "2" #\a #\newline nil "2"))

  ((plet 10 0
     (let ((*print-pretty* T) x)
      (cons
       (with-output-to-string (s)
	(setq x (list
	 (xp::prin1 "2" s)
	 (xp::fresh-line s)
	 (xp::write-line "This is a test" s :start 2)
	 (xp::terpri s)
	 (xp::write-string "This is a test" s :end 7)
	 (multiple-value-list (xp::pprint '(setq a b c d) s)) 
         (xp::write-string "more
tests" s)
	 (xp::print "2" s)
	 (xp::write-char #\a s)
	 (xp::write-char #\newline s)
	 (xp::fresh-line s)
	 (xp::princ "2" s))))
       x)))
   ("\"2\"
is is a test

This is
\(SETQ A B
      C D)more
tests
\"2\" a
2" 
    "2" T "This is a test" nil "This is a test" nil "more
tests" "2" #\a #\newline nil "2"))

  (test-def ((defun bar3 (s item &rest stuff)
	       (declare (ignore stuff))
	       (xp::prin1 (copy-seq item) s)
	       (xp::fresh-line s)
	       (xp::write-line "This is a test" s :start 2)
	       (xp::terpri s)
	       (xp::write-string "This is a test" s :end 7)
	       (xp::pprint '(setq a b c d) s)
	       (xp::write-string "more
tests" s)
	       (xp::print (copy-seq item) s)
	       (xp::write-char #\a s)
	       (xp::write-char #\newline s)
	       (xp::fresh-line s)
	       (xp::princ (copy-seq item) s))
	     (plet 14 0
	       (xp::format nil (formatter "-~/bar3/-") "2")))
	    "-\"2\"
is is a test

This is
\(SETQ A B
      C D)more
tests
\"2\" a
2-")

  (test-def ((defun bar4 (s item &rest stuff)
	       (declare (ignore stuff))
	       (xp::format s (formatter ",~_~A") item)
	       (xp::finish-output s)
	       (xp::format s (formatter ",~_~A") item)
	       (xp::force-output s)
	       (xp::format s (formatter ",~_~A") item)
	       (xp::clear-output s))
	     (plet 100 0
	       (xp::format nil (formatter "-~<a ~_aa~:>+~_~:<b~/bar4/~:>-") () '(2))))
	    "-a aa+
\(b,
 2,
 2)-")

  ((plet 14 0 (progn (setq xp::*format-string-cache* T) (xp::format nil "~A" 4))) "4")
  ((plet 14 0 (xp::format nil "~10<foo~>" 4)) "       foo")
  ((plet 14 0 (xp::format nil "~@<foo~:>" 4)) "foo")
  ((plet 14 0 (xp::format nil "~@<foo~:@>" 4)) "foo")
  ((plet 14 0 (xp::format nil "~w" 4)) "4")
  ((plet 14 0
     (let ((xp::*format-string-cache* nil))
       (xp::format nil "~w" 4))) "4")
  ((plet 14 0
     (let ((string "~W"))
       (list (xp::format nil string 4) (xp::format nil string 5)))) ("4" "5"))
  ((plet 20 0
     (flet ((bar (xp &rest objects)
	      (xp::format xp "~{~A~}" objects)))
       (xp::format nil (formatter "-~?-") #'bar '(1 2 3 4)))) "-1234-")

  ((with-output-to-string (*standard-output*) (xp::prin1 44)) "44")
  ((with-output-to-string (*standard-output*) (xp::prin1 44 nil)) "44")
  ((with-output-to-string (*terminal-io*) (xp::prin1 44 T)) "44")

  ((plet 100 0 (xp::princ-to-string '(setq a "2" b 8))) "(SETQ A 2 B 8)")
  ((plet 100 0
     (let ((*print-pretty* nil))
       (xp::prin1-to-string '(setq a "2" b 8)))) "(SETQ A \"2\" B 8)")
  ((plet 100 0 (xp::write-to-string '(setq a "2" b 8) :base 8 :right-margin 13))
   "(SETQ A \"2\"
      B 10)")

  (deftest
    (progn (xp::defstruct foo (a 1) (b 2))
	   (plet 10 0 (xp::prin1-to-string (make-foo))))
   "#S(FOO :A 1
       :B 2)")
  (deftest
    (progn (xp::defstruct (foo00 (:conc-name nil)) (a 1) (b 2))
	   (plet 10 0 (xp::prin1-to-string (make-foo00))))
   "#S(FOO00 :A 1
         :B 2)")
  (deftest
    (progn (xp::defstruct (foo0 (:constructor mf)) (a 1) (b 2))
	   (plet 11 0 (xp::prin1-to-string (mf))))
   "#S(FOO0 :A 1
        :B 2)")
  (deftest
    (progn (xp::defstruct (foo1 (:conc-name tuz)) a (b 2))
	   (plet 16 0 (xp::prin1-to-string (make-foo1 :a '(1 2 3)))))
   "#S(FOO1 :A (1 2
            3)
        :B 2)")
  (deftest
    (progn (defun foo2p (ob s d)
	     (if (and *print-level* (not (< d *print-level*))) (xp::princ "#" s)
		 (xp::format s (formatter "#<foo2 ~_is ~A>") (aa ob))))
	   (xp::defstruct (foo2 (:conc-name nil) (:print-function foo2p)) (aa 3))
	   (plet 13 0 (list (let ((*printe-level* 1))
			      (xp::format nil (formatter "~W---") (make-foo2)))
			    (let ((*print-level* 0))
			      (xp::format nil (formatter "~W---") (make-foo2)))
			    (with-output-to-string (s)
			      (prin1 (make-foo2) s)
			      (princ "---" s)))))
   ("#<foo2
is 3>---" "#---" "#<foo2 is 3>---"))
  (deftest
    (progn (xp::defstruct (foo3 (:type list)) (a 1) (b 2))
	   (prints (make-foo3))))
  (deftest
    (progn (xp::defstruct (foo4 (:include foo2)) (e 1))
	   (prints (make-foo4))))

;Tests of things about dispatching

  ((progn (setq *dt* (copy-pprint-dispatch nil)) nil))
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member zotz))
	#'(lambda (xp list)
	    (format xp (formatter "~@{~@<ZOTZ-~W~:>~}") (cadr list))) 0 *dt*)
      (plet 30 0 (let ((*print-shared* t))
		   (xp::format nil (formatter "~W")
			       '((zotz 1) (zotz 2) (zotz 3))))))
   "(ZOTZ-1 ZOTZ-2 ZOTZ-3)")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member zotz))
	'(lambda (xp list)
	   (format xp (formatter "~@<ZOTZ-~W~:>") (cadr list))) 0 *dt*)
      (plet 60 0 (let ((*print-shared* t))
		   (xp::format nil (formatter "~W")
			       (read-from-string
				 "(#1=(zotz 1) #1# (zotz (1 #2=(2 #2#))))")))))
   "(#1=ZOTZ-1 #1# ZOTZ-(1 #2=(2 #2#)))")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member zotz))
	#'(lambda (xp list)
	    (format xp (formatter "~<~*ZOTZ-~W~:>") list)) 0 *dt*)
      (plet 30 0 (let ((*print-shared* t))
		   (xp::format nil (formatter "~W")
			       '((zotz 1) (zotz 2) (zotz 3))))))
   "(ZOTZ-1 ZOTZ-2 ZOTZ-3)")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member zotz))
	#'(lambda (xp list)
	    (format xp (formatter "~<~*ZOTZ-~W~:>") list)) 0 *dt*)
      (plet 30 0 (let ((*print-shared* t))
		   (xp::format nil (formatter "~W")
			       (read-from-string
				 "((zotz 1) #1=(zotz 2) #1#)")))))
   "(ZOTZ-1 #1=ZOTZ-2 #1#)")
  
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch 'null (formatter "()"))
      (plet 20 0 (xp::format nil (formatter "~W") nil)))
   "()")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (not (satisfies numberp))
				  (cons (member a b c)))
			   (formatter "~{~a+~a~}"))
      (plet 20 0 (xp::format nil (formatter "~W") '(a a))))
   "A+A")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (cons) (cons (member a)))
			   (formatter "~{~a-~a~}") 1)
      (plet 20 0 (xp::format nil (formatter "~W") '((a) a))))
   "(A)-A")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch 'hash-table (formatter "foof"))
      (plet 20 0 (xp::format nil (formatter "~W") (make-hash-table))))
   "foof")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(and integer (satisfies evenp)) (formatter "+~D") 3 *dt*)
      (plet 20 0 (xp::format nil (formatter "~W") '(1 2 3 4))))
   "(1 +2 3 +4)")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(and (member 10 11 12)) (formatter "**~D") 20 *dt*)
      (plet 20 0 (xp::format nil (formatter "~W") '(1 12 3 11))))
   "(1 **12 3 **11)")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(and (member 10 11 12)) nil 0 *dt*)
      (plet 20 0 (xp::format nil (formatter "~W") '(1 12 3 11))))
   "(1 +12 3 11)")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(vector * 4)
	#'(lambda (xp obj)
	    (xp::format xp (formatter "--~S") (coerce obj 'list))))
      (plet 20 0 (xp::format nil (formatter "~W") '#(k l d a))))
   "--(K L D A)")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member unwind-protect))
	#'(lambda (xp list)
	    (xp::print-fancy-fn-call xp list '(0 3 1 0))))
      (plet 20 0 (xp::format nil (formatter "~W")
			     '(unwind-protect (open f) (print errormsg)))))
   "(UNWIND-PROTECT
    (OPEN F)
 (PRINT ERRORMSG))")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member unwind-protect))
			   (pprint-dispatch '(unwind-protect) nil))
      (plet 20 0 (xp::format nil (formatter "~W")
			     '(unwind-protect (open f) (print errormsg)))))
   "(UNWIND-PROTECT
    (OPEN F)
  (PRINT ERRORMSG))")
#-symbolics
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member unwind-protect)) nil)
      (plet 30 1 (xp::format nil (formatter "~W")
			     '(unwind-protect (open f) (print errormsg)))))
   "(UNWIND-PROTECT (OPEN F)
                (PRINT ERRORMSG))") ;note zwei: offset hash table
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch 'cons (formatter "zot ~{~A ~}") 1)
      (plet 20 0 (xp::format nil (formatter "~W") '(setq a b))))
   "zot SETQ A B ")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch 'cons (pprint-dispatch '(22 . 33) nil) 1)
      (plet 30 0 (xp::format nil (formatter "~W")
			     '(unwind-protect (open f) (print errormsg)))))
   "(UNWIND-PROTECT (OPEN F)
 (PRINT ERRORMSG))")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons) (formatter "zoz ~{~A ~}") 2)
      (plet 100 0 (xp::format nil (formatter "~W") '(foo bar))))
   "zoz FOO BAR ")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons integer) (formatter "int ~{~A ~}") 3)
      (plet 100 0 (xp::format nil (formatter "~W") '(3 bar))))
   "int 3 BAR ")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member a b)) (formatter "pip ~{~A ~}") 3)
      (plet 100 0 (xp::format nil (formatter "~W") '(a bar))))
   "pip A BAR ")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member)) (formatter "pop ~{~A~}") 4)
      (plet 100 0 (xp::format nil (formatter "~W") '(a bar))))
   "pip A BAR ")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (let ((data (make-foo :a 10 :b 20)))
	(plet 22 0 (xp::format nil (formatter "~W") data))))
    "#S(FOO :A +10 :B +20)")
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch 'foo
	#'(lambda (xp obj)
	    (xp::format xp (formatter "foo-~A-~A") (foo-a obj) (foo-b obj))))
      (let ((data (make-foo :a 11 :b 21)))
	(plet 20 0 (xp::format nil (formatter "~W") data))))
    "foo-11-21")

;Tests of the functional interface to the dynamic formatting things.

  ((progn (setq *dt* (copy-pprint-dispatch nil)) nil))
  (deftest
    (let ((*print-pprint-dispatch* *dt*))
      (set-pprint-dispatch '(cons (member setz))
        #'(lambda (*standard-output* list)
	    (pprint-logical-block (nil list :prefix "[" :suffix "]")
	      (pprint-pop)
	      (write-string "SETZ")
	      (pprint-exit-if-list-exhausted)
	      (write-char #\space)
	      (pprint-indent :current 0)
	      (pprint-newline :miser)
	      (loop (write (pprint-pop))
		    (pprint-exit-if-list-exhausted)
		    (write-char #\space)
		    (pprint-newline :fill)
		    (write (pprint-pop))
		    (pprint-exit-if-list-exhausted)
		    (write-char #\space)
		    (pprint-newline :linear)))))
      nil))
  ((ftest 100 0 '(setz a (car v) b (cdr v)) (*print-pprint-dispatch* *dt*))
   "[SETZ A (CAR V) B (CDR V)]")
  ((ftest 20 0 '(setz a (car v) b (cdr v)) (*print-pprint-dispatch* *dt*))
   "[SETZ A (CAR V)
      B (CDR V)]")
  ((ftest 20 20 '(setz a (car v) b (cdr v)) (*print-pprint-dispatch* *dt*))
   "[SETZ
 A
 (CAR V)
 B
 (CDR V)]")
  ((ftest 17 0 '(setz a (car v) b) (*print-pprint-dispatch* *dt*))
   "[SETZ A (CAR V)
      B]")
  ((ftest 17 0 '(setz a (car v) . b) (*print-pprint-dispatch* *dt*))
   "[SETZ A (CAR V)
      . B]")
  ((ftest 100 0 '(setz . a) (*print-pprint-dispatch* *dt*))
   "[SETZ . A]")
  ((ftest 100 0 '(setz a (car v) b (cdr v))
	  (*print-pprint-dispatch* *dt*) (*print-length* 2))
   "[SETZ A ...]")  
  ((ftest 100 0 '(setz a (car v) b (cdr v))
	  (*print-pprint-dispatch* *dt*) (*print-level* 1))
   "[SETZ A # B #]")
  ((ftest 100 0 '((setz a (car v) b (cdr v)))
	  (*print-pprint-dispatch* *dt*) (*print-level* 1))
   "(#)")
  ((ftest 100 0 '(setz a (car v) b (cdr v))
	  (*print-pprint-dispatch* *dt*) (*print-circle* t))
   "[SETZ A (CAR V) B (CDR V)]")
  ((ftest 100 0 (read-from-string "(setz a #1=(car v) b #1#)")
	  (*print-pprint-dispatch* *dt*) (*print-circle* t) (*print-shared* T))
   "[SETZ A #1=(CAR V) B #1#]")
  ((ftest 100 0 (read-from-string "(setz a #1=(car v) b #1#)")
	  (*print-pprint-dispatch* *dt*) (*print-circle* t) (*print-shared* nil))
   "[SETZ A (CAR V) B (CAR V)]")
  ((ftest 100 0 (read-from-string "#1=(setz a (car v) b #1#)")
	  (*print-pprint-dispatch* *dt*) (*print-circle* t) (*print-shared* T))
   "#1=[SETZ A (CAR V) B #1#]")
  ((ftest 100 0 (read-from-string "#1=(setz a (car v) b #1#)")
	  (*print-pprint-dispatch* *dt*) (*print-circle* t) (*print-shared* nil))
   "#1=[SETZ A (CAR V) B #1#]")

  ((plet 16 0
     (with-output-to-string (*terminal-io*)
       (let ((list '(setz a (car v) b (cdr v))))
	(pprint-logical-block (*terminal-io* list :per-line-prefix "[")
	  (pprint-pop)
	  (write-string "SETZ" T)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space T)
	  (pprint-indent :current 0 T)
	  (pprint-newline :miser T)
	  (loop (write (pprint-pop) :stream T)
		(pprint-exit-if-list-exhausted)
		(write-char #\space T)
		(pprint-newline :fill T)
		(write (pprint-pop) :stream  T)
		(pprint-exit-if-list-exhausted)
		(write-char #\space T)
		(pprint-newline :linear T))))))
   "[SETZ A (CAR V)
[     B (CDR V)")

  ((plet 100 0
     (with-output-to-string (*standard-output*)
       (let ((list '(setz a (car v) b (cdr v))))
	 (pop list)
	 (write-string "SETZ")
	 (when list
	   (write-char #\space)
	   (pprint-indent :current 0)
	   (pprint-tab :line 0 0)
	   (pprint-newline :miser)
	   (loop (write (pop list))
		 (if (null list) (return nil))
		 (write-char #\space)
		 (pprint-newline :fill)
		 (write (pop list))
		 (if (null list) (return nil))
		 (write-char #\space)
		 (pprint-newline :linear))))))
   "SETZ A (CAR V) B (CDR V)")

  (deftest
    (progn (defun my-pprint-tabular (s list &optional (colon? T) atsign? (tabsize nil))
	     (declare (ignore atsign?))
	     (if (null tabsize) (setq tabsize 16))
	     (pprint-logical-block (s list :prefix (if colon? "(" "")
				           :suffix (if colon? ")" ""))
	       (pprint-exit-if-list-exhausted)
	       (loop (write (pprint-pop) :stream s)
		     (pprint-exit-if-list-exhausted)
		     (write-char #\space s)
		     (pprint-tab :section-relative 0 tabsize s)
		     (pprint-newline :fill s))))
	   nil))
  ((plet 100 0 (xp::format nil (formatter "---~5/my-pprint-tabular/--")
		       '(12 3456 789 22 45656 78)))
   "---12   3456 789  22   45656     78--")
  ((plet 100 0
     (let ((*print-length* 3))
       (xp::format nil (formatter "---~5/my-pprint-tabular/--")
		  '(12 3456 789 22 456 78))))
   "---12   3456 789  ...--")
  ((plet 21 0 (xp::format nil (formatter "---~5/my-pprint-tabular/--")
		       '(12 3456 789 22 456 78)))
   "---12   3456 789
   22   456  78--")
  ((plet 21 0 (xp::format nil (formatter "---~5:/my-pprint-tabular/--")
		       '(12 3456 789 22 456 78)))
   "---(12   3456 789
    22   456  78)--")
  ((plet 21 0 (xp::format nil (formatter "---~5:/my-pprint-tabular/--")
		       '(12 3456 789 22 456 78)))
   "---(12   3456 789
    22   456  78)--")
  ((plet 100 0
     (let ((*print-length* 3))
       (xp::format nil (formatter "---~5:/my-pprint-tabular/--")
		  '(12 3456 789 22 456 78))))
   "---(12   3456 789  ...)--")
  ((plet 41 0 (xp::format nil (formatter "---~V/my-pprint-tabular/--")
		       nil '(12 3456 789 22 456 78)))
   "---12              3456
   789             22
   456             78--")
  ((plet 21 0 (xp::format nil (formatter "---~5:/my-pprint-tabular/--") ()))
   "---()--")
  ((plet 21 0 (xp::format nil (formatter "---~5:/my-pprint-tabular/--") 12))
   "---12--")
  ((plet 21 0 (let ((*print-level* 0)) (xp::format nil (formatter "---~5/my-pprint-tabular/--") ())))
   "---#--")
  ((plet 21 0 (let ((*print-level* 0)) (xp::format nil (formatter "---~5:/my-pprint-tabular/--") ())))
   "---#--")

;tests of formats for various special forms.

  ((progn (setq *print-pprint-dispatch* (copy-pprint-dispatch)) nil))

  ((ftest 15 0 '(cons aaaaaa bbbbb))
   "(CONS AAAAAA
      BBBBB)")
  ((ftest 15 0 '(cons aaaaaa (cons a b)) (*print-level* 1))
   "(CONS AAAAAA #)")

  ((ftest 100 0 '(block foo (if (null y) (return T)) x))
   "(BLOCK FOO (IF (NULL Y) (RETURN T)) X)")
  ((ftest 30 0 '(block foo (if (null y) (return T)) x))
   "(BLOCK FOO
  (IF (NULL Y) (RETURN T))
  X)")
  ((ftest 30 40 '(block foo (if (null y) (return T)) x))
   "(BLOCK
 FOO
 (IF (NULL Y) (RETURN T))
 X)")
  ((ftest 100 0 '(block foo . T))
   "(BLOCK FOO . T)")
  ((ftest 100 0 '(block . T))
   "(BLOCK . T)")
  ((ftest 100 0 '((block . T)) (*print-level* 1))
   "(#)")

  ((ftest 20 0 '(case type (:foo (print 3))))
   "(CASE TYPE
  (:FOO (PRINT 3)))")
  ((ftest 10 0 '(catch 'bar (foo x)))
   "(CATCH 'BAR
  (FOO X))")
  ((ftest 20 0 '(ccase type (:foo (print 3))))
   "(CCASE TYPE
  (:FOO (PRINT 3)))")

  ((ftest 100 0 '(compiler-let ((a (foo 3)) (b (foo 4)) (c 1)) (tuz a b)))
   "(COMPILER-LET ((A (FOO 3)) (B (FOO 4)) (C 1)) (TUZ A B))")
  ((ftest 50 0 '(compiler-let ((a (foo 3)) (b (foo 4)) (c 1)) (tuz a b)))
   "(COMPILER-LET ((A (FOO 3)) (B (FOO 4)) (C 1))
  (TUZ A B))")
  ((ftest 44 0 '(compiler-let ((a (foo 3)) (b (foo 4)) (c 1)) (tuz a b)))
   "(COMPILER-LET ((A (FOO 3))
               (B (FOO 4))
               (C 1))
  (TUZ A B))")
  ((ftest 44 50 '(compiler-let ((a (foo 3)) (b (foo 4)) (c 1)) (tuz a b)))
   "(COMPILER-LET
 ((A (FOO 3)) (B (FOO 4)) (C 1))
 (TUZ A B))")
  ((ftest 30 0 '(compiler-let (bar baz def gack gortch) (tuz a b)))
   "(COMPILER-LET (BAR BAZ DEF
               GACK GORTCH)
  (TUZ A B))")
  ((ftest 40 0 '(compiler-let ((bar baz def gack . gortch))))
   "(COMPILER-LET ((BAR BAZ DEF GACK
                . GORTCH)))")
  ((ftest 100 0 '(compiler-let (bar baz def gack . gortch)))
   "(COMPILER-LET (BAR BAZ DEF GACK . GORTCH))")
  ((ftest 40 0 '(compiler-let foo))
   "(COMPILER-LET FOO)")
  ((ftest 40 0 '(compiler-let ()))
   "(COMPILER-LET ())")
  ((ftest 40 0 '(compiler-let . foo))
   "(COMPILER-LET . FOO)")

  ((ftest 100 0 '(cond ((plusp x) (print x) . 4) (a b) (T (car x))))
   "(COND ((PLUSP X) (PRINT X) . 4) (A B) (T (CAR X)))")
  ((ftest 55 0 '(cond ((plusp x) (print x) (minus x)) (a b) (T (car x))))
   "(COND ((PLUSP X) (PRINT X) (MINUS X))
      (A B)
      (T (CAR X)))")
  ((ftest 36 0 '(cond ((plusp x) (print x) (minus x)) (a b) (T (car x))))
   "(COND ((PLUSP X)
       (PRINT X)
       (MINUS X))
      (A B)
      (T (CAR X)))")
  ((ftest 30 40 '(cond ((plusp x) (print x) (minus x)) (a b) (T (car x))))
   "(COND
 ((PLUSP X)
  (PRINT X)
  (MINUS X))
 (A B)
 (T (CAR X)))")
  ((ftest 10 40 '(cond (a b) . T))
   "(COND
 (A B)
 . T)")
  ((ftest 10 40 '(cond))
"(COND)")

  ((ftest 20 0 '(ctypecase type (:foo (print 3))))
   "(CTYPECASE TYPE
  (:FOO (PRINT 3)))")
  ((ftest 20 0 '(defconstant foo 2 "test"))
   "(DEFCONSTANT FOO 2
  \"test\")")
  ((ftest 30 0 '(defconstant foo 2 (defconstant)) (*print-level* 1))
   "(DEFCONSTANT FOO 2 #)")
  ((ftest 40 0 '(define-setf-method ldb (a b) (make-right body a b)))
   "(DEFINE-SETF-METHOD LDB (A B)
  (MAKE-RIGHT BODY A B))")

  ((ftest 100 0 '(defmacro foo (a (b c) &body d) (car a) (list b c)))
   "(DEFMACRO FOO (A (B C) &BODY D) (CAR A) (LIST B C))")
  ((ftest 40 0 '(defmacro foo (a (b c) &body d) (car a) (list b c)))
   "(DEFMACRO FOO (A (B C) &BODY D)
  (CAR A)
  (LIST B C))")
  ((ftest 25 0 '(defmacro foo (a (b c) &body d) (car a) (list b c)))
   "(DEFMACRO FOO (A (B C)
               &BODY D)
  (CAR A)
  (LIST B C))")
  ((ftest 15 50 '(defmacro foo (a (b c) &body d) (car a) (list b c)))
   "(DEFMACRO
 FOO
 (A
  (B C)
  &BODY
  D)
 (CAR A)
 (LIST B C))")
  ((ftest 100 0 '(defmacro foo () . t))
   "(DEFMACRO FOO () . T)")
  ((ftest 100 0 '(defmacro . foo))
   "(DEFMACRO . FOO)")

  ((ftest 100 0 '(define-modify-macro bar (a b) union "fancy union"))
   "(DEFINE-MODIFY-MACRO BAR (A B) UNION \"fancy union\")")
  ((ftest 40 0 '(define-modify-macro bar (a b) union "fancy union"))
   "(DEFINE-MODIFY-MACRO BAR (A B) UNION
  \"fancy union\")")
  ((ftest 30 0 '(define-modify-macro bar (a b) union "fancy union"))
   "(DEFINE-MODIFY-MACRO BAR
                     (A B)
                     UNION
  \"fancy union\")")
  ((ftest 100 0 '(define-modify-macro bar (a b) union . T))
   "(DEFINE-MODIFY-MACRO BAR (A B) UNION . T)")
  ((ftest 100 0 '(define-modify-macro bar args . T))
   "(DEFINE-MODIFY-MACRO BAR ARGS . T)")
  ((ftest 100 0 '(define-modify-macro bar . T))
   "(DEFINE-MODIFY-MACRO BAR . T)")
  ((ftest 100 0 '(define-modify-macro . T))
   "(DEFINE-MODIFY-MACRO . T)")
  ((print*c "#1=(define-modify-macro foo #1#)")
   "#1=(DEFINE-MODIFY-MACRO FOO #1#)")

  ((ftest 20 0 '(defparameter foo 2 "test"))
   "(DEFPARAMETER FOO 2
  \"test\")")
  ((ftest 40 0 '(defsetf bar (a b) (store) (car x) (make-body a b)))
   "(DEFSETF BAR (A B) (STORE)
  (CAR X)
  (MAKE-BODY A B))")
  ((ftest 20 0 '(defsetf bar (a b) (store) (car x) (make-body a b)))
   "(DEFSETF BAR (A B)
         (STORE)
  (CAR X)
  (MAKE-BODY A B))")
  ((ftest 40 0 '(define-setf-method bar (a b) (car x) (make-body a b)))
   "(DEFINE-SETF-METHOD BAR (A B)
  (CAR X)
  (MAKE-BODY A B))")
  ((ftest 40 0 '(defstruct (foo (:print-fn bar)) acac babab))
   "(DEFSTRUCT (FOO (:PRINT-FN BAR))
  ACAC
  BABAB)")
  ((ftest 30 0 '(deftype bar (a) (satisfies bar-p)))
   "(DEFTYPE BAR (A)
  (SATISFIES BAR-P))")
  ((ftest 30 0 '(defun bar (a) (satisfies bar-p)))
   "(DEFUN BAR (A)
  (SATISFIES BAR-P))")
  ((ftest 20 0 '(defvar foo 2 "test"))
   "(DEFVAR FOO 2
  \"test\")")

  ((ftest 100 0 '(do ((a x (cdr a)) (i 1 (1+ i))) ((plusp a) T) (print a)))
   "(DO ((A X (CDR A)) (I 1 (1+ I))) ((PLUSP A) T)  (PRINT A))")
  ((ftest 55 0 '(do ((a x (cdr a)) (i 1 (1+ i))) ((plusp a) T) (print a)))
   "(DO ((A X (CDR A)) (I 1 (1+ I)))
    ((PLUSP A) T)
  (PRINT A))")
  ((ftest 30 0 '(do ((a x (cdr a)) (i 1 (1+ i))) ((plusp a) T) (print a)))
   "(DO ((A X (CDR A))
     (I 1 (1+ I)))
    ((PLUSP A) T)
  (PRINT A))")
  ((ftest 15 0 '(do ((a x (cdr a)) (i 1 (1+ i))) ((plusp a) T) (print a)))
   "(DO ((A X
      (CDR A))
     (I 1
      (1+ I)))
    ((PLUSP A)
     T)
  (PRINT A))")
  ((ftest 15 20 '(do ((a x (cdr a)) (i 1 (1+ i))) ((plusp a) T) (print a)))
   "(DO
 ((A
   X
   (CDR A))
  (I
   1
   (1+ I)))
 ((PLUSP A)
  T)
 (PRINT A))")
  ((ftest 100 0 '(do () () . T))
   "(DO () ()  . T)")
  ((ftest 100 0 '(do () . T))
   "(DO () . T)")
  ((ftest 100 0 '(do . T))
   "(DO . T)")

  ((ftest 55 0 '(do* ((a x (cdr a)) (i 1 (1+ i))) ((plusp a) T) (print a)))
   "(DO* ((A X (CDR A)) (I 1 (1+ I)))
     ((PLUSP A) T)
  (PRINT A))")
  ((ftest 35 0 '(do-all-symbols (s *package*) (print s)))
   "(DO-ALL-SYMBOLS (S *PACKAGE*)
  (PRINT S))")
  ((ftest 35 0 '(do-external-symbols (s *package*) (print s)))
   "(DO-EXTERNAL-SYMBOLS (S *PACKAGE*)
  (PRINT S))")
  ((ftest 35 0 '(do-symbols (s *package*) (print s)))
   "(DO-SYMBOLS (S *PACKAGE*)
  (PRINT S))")
  ((ftest 25 0 '(dolist (s list) (print s)))
   "(DOLIST (S LIST)
  (PRINT S))")
  ((ftest 25 0 '(dotimes (s list) (print s)))
   "(DOTIMES (S LIST)
  (PRINT S))")

  ((ftest 20 0 '(ecase type (:foo (print 3))))
   "(ECASE TYPE
  (:FOO (PRINT 3)))")
  ((ftest 20 0 '(etypecase type (:foo (print 3))))
   "(ETYPECASE TYPE
  (:FOO (PRINT 3)))")
  ((ftest 20 0 '(eval-when (compile load) (defun foo () (car x))))
   "(EVAL-WHEN (COMPILE LOAD)
  (DEFUN FOO ()
    (CAR X)))")

  ((ftest 100 0 '(flet ((a (a b) (car a) (car b)) (b () t)) (a (b 3))))
   "(FLET ((A (A B) (CAR A) (CAR B)) (B NIL T)) (A (B 3)))")
  ((ftest 50 0 '(flet ((a (a b) (car a) (car b)) (b () t)) (a (b 3))))
   "(FLET ((A (A B) (CAR A) (CAR B)) (B NIL T))
  (A (B 3)))")
  ((ftest 42 0 '(flet ((a (a b) (car a) (car b)) (b () t)) (a (b 3))))
   "(FLET ((A (A B) (CAR A) (CAR B))
       (B NIL T))
  (A (B 3)))")
  ((ftest 30 0 '(flet ((a (a b) (car a) (car b)) (b () t)) (a (b 3))))
   "(FLET ((A (A B)
         (CAR A)
         (CAR B))
       (B NIL T))
  (A (B 3)))")
  ((ftest 35 50 '(flet ((a (a b) (car a) (car b)) (b () t)) (a (b 3))))
   "(FLET
 ((A (A B) (CAR A) (CAR B))
  (B NIL T))
 (A (B 3)))")
  ((ftest 100 0 '(flet (() T . T) . T))
   "(FLET (() T . T) . T)")
  ((ftest 100 0 '(flet T . T))
   "(FLET T . T)")
  ((ftest 100 0 '(flet . T))
   "(FLET . T)")

  ((ftest 100 0 '(function (lambda (a) (car a))))
   "#'(LAMBDA (A) (CAR A))")
  ((ftest 100 0 '(function (lambda (a) (car a))) (*print-pretty* nil))
   "(FUNCTION (LAMBDA (A) (CAR A)))")
  ((ftest 5 20 '(function car))
   "#'CAR")
  ((ftest 100 0 '(function . a))
   "(FUNCTION . A)")
  ((ftest 100 0 '(function))
   "(FUNCTION)")
  ((ftest 100 0 '(function (lambda (a) (car a)) b))
   "(FUNCTION (LAMBDA (A) (CAR A)) B)")

  ((ftest 42 0 '(labels ((a (a b) (car a) (car b)) (b () t)) (a (b 3))))
   "(LABELS ((A (A B) (CAR A) (CAR B))
         (B NIL T))
  (A (B 3)))")
  ((ftest 20 0 '(lambda (a b) (car a) (car b)))
   "(LAMBDA (A B)
  (CAR A)
  (CAR B))")

  ((ftest 34 0 '(let ((a (foo 3)) (b (foo 4)) (c 1)) (tuz a b)))
   "(LET ((A (FOO 3))
      (B (FOO 4))
      (C 1))
  (TUZ A B))")
  ((ftest 34 0 '(let* ((a (foo 3)) (b (foo 4)) (c 1)) (tuz a b)))
   "(LET* ((A (FOO 3))
       (B (FOO 4))
       (C 1))
  (TUZ A B))")
  ((ftest 34 0 '(locally (declar (special x)) (print x)))
   "(LOCALLY (DECLAR (SPECIAL X))
  (PRINT X))")
  ((ftest 42 0 '(macrolet ((a (a b) (car a) (car b)) (b () t)) (a (b 3))))
   "(MACROLET ((A (A B) (CAR A) (CAR B))
           (B NIL T))
  (A (B 3)))")
  ((ftest 42 0 '(multiple-value-bind (a b) (compute-it x) (car a) (car b)))
   "(MULTIPLE-VALUE-BIND (A B)
    (COMPUTE-IT X)
  (CAR A)
  (CAR B))")
  ((ftest 32 0 '(multiple-value-setq (a b) (compute-it x)))
   "(MULTIPLE-VALUE-SETQ (A B)
  (COMPUTE-IT X))")

  ((ftest 100 0
     '(prog (a b c) (print a) L SS (if (null b) c) long0 (car b) long))
   "(PROG (A B C)
      (PRINT A)
 L SS (IF (NULL B) C)
 LONG0 (CAR B)
 LONG)")
  ((ftest 100 100
     '(prog (a b c) (print a) L SS (if (null b) c) long0 (car b) long))
   "(PROG (A B C)
      (PRINT A)
 L SS (IF (NULL B) C)
 LONG0 (CAR B)
 LONG)")
  ((ftest 100 0 '(prog () (print a) nil L . SS))
   "(PROG ()
      (PRINT A)
      NIL
 L . SS)")
  ((ftest 100 0 '(prog T . T))
   "(PROG T . T)")
  ((ftest 100 0 '(prog . T))
   "(PROG . T)")

  ((ftest 100 0 '(prog* ((a 3) b c) L SS (if (null b) c) long0 (car b)))
   "(PROG* ((A 3) B C)
 L SS  (IF (NULL B) C)
 LONG0 (CAR B))")
  ((ftest 25 0 '(progv (a b) (1 2) (car a)))
   "(PROGV (A B) (1 2)
  (CAR A))")

  ((ftest 20 0 '(setq a (car v) b (cdr v)))
   "(SETQ A (CAR V)
      B (CDR V))")
  ((ftest 20 20 '(setq a (car v) b (cdr v)))
   "(SETQ
 A
 (CAR V)
 B
 (CDR V))")
  ((ftest 17 0 '(setq a (car v) b))
   "(SETQ A (CAR V)
      B)")
  ((ftest 17 0 '(setq a (car v) . b))
   "(SETQ A (CAR V)
      . B)")
  ((ftest 100 0 '(setq . a))
   "(SETQ . A)")

  ((ftest 100 0 '(quote (lambda (a) (car a))))
   "'(LAMBDA (A) (CAR A))")
  ((ftest 5 20 '(quote car))
   "'CAR")
  ((ftest 100 0 '(quote . a))
   "(QUOTE . A)")
  ((ftest 100 0 '(quote))
   "(QUOTE)")
  ((ftest 100 0 '(quote (lambda (a) (car a)) b))
   "(QUOTE (LAMBDA (A) (CAR A)) B)")

  ((ftest 20 0 '(return-from foo (computation bar)))
   "(RETURN-FROM FOO
  (COMPUTATION BAR))")

  ((ftest 20 0 '(setf a (car v) b (cdr v)))
   "(SETF A (CAR V)
      B (CDR V))")
  ((ftest 1000 0 '(setf a (car v) b (cdr v)))
   "(SETF A (CAR V) B (CDR V))")
  ((ftest 20 0 '(psetf a (car v) b (cdr v)))
   "(PSETF A (CAR V)
       B (CDR V))")
  ((ftest 20 0 '(psetq a (car v) b (cdr v)))
   "(PSETQ A (CAR V)
       B (CDR V))")

  ((ftest 100 0
     '(tagbody (print a) L SS (if (null b) c) verylong (car b) long))
   "(TAGBODY (PRINT A)
 L SS    (IF (NULL B) C)
 VERYLONG (CAR B)
 LONG)")
  ((ftest 100 0 '(tagbody L SS (if (null b) c) . T))
   "(TAGBODY
 L SS    (IF (NULL B) C) . T)")
  ((ftest 100 0 '(tagbody L . SS))
   "(TAGBODY
 L . SS)")
  ((ftest 100 0 '(tagbody . SS))
   "(TAGBODY . SS)")
  ((ftest 10 0 '(throw 'bar (foo x)))
   "(THROW 'BAR
  (FOO X))")
  ((ftest 20 0 '(typecase type (:foo (print 3))))
   "(TYPECASE TYPE
  (:FOO (PRINT 3)))")
  ((ftest 20 0 '(unless (plusp x) (print x)))
   "(UNLESS (PLUSP X)
  (PRINT X))")
  ((ftest 20 0 '(unwind-protect (open f) (print errormsg)))
   "(UNWIND-PROTECT
    (OPEN F)
  (PRINT ERRORMSG))")
  ((ftest 20 0 '(when (plusp x) (print x)))
   "(WHEN (PLUSP X)
  (PRINT X))")
  ((ftest 35 0 '(with-input-from-string (f string) (read f)))
   "(WITH-INPUT-FROM-STRING (F STRING)
  (READ F))")
  ((ftest 45 0 '(with-open-file (f name :direction :input) (read f)))
   "(WITH-OPEN-FILE (F NAME :DIRECTION :INPUT)
  (READ F))")
  ((ftest 45 0 '(with-open-stream (stream (make-stream)) (read stream)))
   "(WITH-OPEN-STREAM (STREAM (MAKE-STREAM))
  (READ STREAM))")
  ((ftest 35 0 '(with-output-to-string (f string) (print f)))
   "(WITH-OUTPUT-TO-STRING (F STRING)
  (PRINT F))")

;These test the fast printing of simple atoms performed directly by XP

  ((print*s "foo") "\"foo\"")
  ((print*s "fo
o") "\"fo
o\"")
  ((print*s "foo" (*print-escape* nil)) "foo")
  ((print*s "fo\"o") "\"fo\\\"o\"")
  ((print*s "fo\"o" (*print-escape* nil)) "fo\"o")
  ((print*s "fo\\o") "\"fo\\\\o\"")
  ((print*s "fo\\o" (*print-escape* nil)) "fo\\o")

  ((print*s 20) "20")
  ((print*s 20 (*print-base* 8)) "24")
  ((print*s 20 (*print-radix* T)) "20.")
  ((print*s 1/2) "1/2")
  ((print*s 12345678901234567890) "12345678901234567890")
  ((print*s -20) "-20")
  ((print*s 1234567890) "1234567890")

  ((print*s 'foo2) "FOO2")
  ((print*s 'foo-bar (*print-case* :downcase)) "foo-bar")
  ((print*s 'foo-bar (*print-case* :upcase)) "FOO-BAR")
  ((print*s 'foo-bar (*print-case* :capitalize)) "Foo-Bar")
  ((print*s 'foo (*print-escape* nil)) "FOO")
  ((print*s ':foo) ":FOO")
  ((print*s ':foo (*print-escape* nil)) "FOO")
  ((print*s '*<-+->*) "*<-+->*")
  ((print*s '*<-+->*) "*<-+->*")

  ((prints 'xp::print-fixnum))
  ((prints '\fo\o-bar (*print-escape* nil) (*print-case* :capitalize)))
  ((prints '\fo\o-bar (*print-escape* nil) (*print-case* :downcase)))
  ((prints '\fo\o-bar (*print-escape* nil) (*print-case* :upcase)))
  ((prints '||))
  ((prints '|abcdef|))
  ((prints '|4a|))
  ((prints 'A%))
  ((prints '%))
  ((prints '#*10101))
  ((prints '#2A((12 3456 789) (22 456 78)) (*print-array* nil)))

;Tests of circularity printing.

  ((print*c "(A A NIL NIL B B)" (*print-shared* T))
            "(A A NIL NIL B B)")
  ((print*c "(Ac$ A NIL NIL B B)" (*print-right-margin* 15))
   "(AC$ A NIL NIL
 B B)")
  ((print*c "(1 #1=#:FOO #1# #1# . #1#)" (*print-shared* T))
            "(1 #1=#:FOO #1# #1# . #1#)")
  ((print*c "(1 #1=#:FOO #1# #1# . #1#)" (*print-shared* nil))
            "(1 #:FOO #:FOO #:FOO . #:FOO)")
  ((print*c "(1 #1=#:FOO #1# . #1#)" (*print-gensym* nil))
            "(1 FOO FOO . FOO)")
  ((print*c "(1 #1=#:FOO #1# . #1#)" (*print-length* 2) (*print-shared* T))
            "(1 #:FOO ...)")
  ((print*c "(0 (#1=(1 2) B) #1# . #1#)" (*print-shared* T))
            "(0 (#1=(1 2) B) #1# . #1#)")
  ((print*c "(0 (#1=(1 2) B) #1# . #1#)" (*print-shared* nil))
            "(0 ((1 2) B) (1 2) 1 2)")
  ((print*c "#1=#(0 (1 #2=(A B) #1#) #2#)" (*print-shared* T))
            "#1=#(0 (1 #2=(A B) #1#) #2#)")
  ((print*c "#1=#(0 (1 #2=(A B) #1#) #2#)" (*print-shared* nil))
            "#1=#(0 (1 (A B) #1#) (A B))")
  ((print*c "(COND #1=((PLUSP X) Y) (X Y) . #1#)" (*print-shared* T))
            "(COND #1=((PLUSP X) Y) (X Y) . #1#)")
  ((print*c "(COND #1=((PLUSP X) Y) (X Y) . #1#)" (*print-shared* nil))
            "(COND ((PLUSP X) Y) (X Y) (PLUSP X) Y)")
  ((print*c "(A (B . #1=(C D)) #1# . #1#)" (*print-shared* T))
            "(A (B . #1=(C D)) #1# . #1#)")
  ((print*c "(A (B . #1=(C D)) #1# . #1#)" (*print-shared* nil))
            "(A (B C D) (C D) C D)")
  ((print*c "(A (B . #1=(C #1#)) #1#)" (*print-shared* T))
            "(A (B . #1=(C #1#)) #1#)")
  ((print*c "(A (B . #1=(C #1#)) #1#)" (*print-shared* nil))
            "(A (B . #1=(C #1#)) #1#)")
  ((print*c "(A (B . #1=(C . #2=(D E))) #2# #1# F)" (*print-shared* T))
   "(A (B . #2=(C . #1=(D E))) #1# #2# F)")
  ((print*c "(A (B . #1=(C . #2=(D E))) #2# #1# F)" (*print-shared* nil))
   "(A (B C D E) (D E) (C D E) F)")
  ((print*c "(A (B . #1=(C . #2=(D E))) #2# #1# F)" (*print-level* 2) (*print-shared* T))
   "(A (B . #2=(C . #1=(D E))) #1# #2# F)")
  ((print*c "(A ((B . #1=(C . #2=(D E)))) #2# #1# F)" (*print-level* 2) (*print-shared* T))
   "(A (#) #1=(D E) (C . #1#) F)")
  ((print*c "(setq A #1=(car f) B C D #1#)"
	    (*print-lines* 2) (*PRINT-RIGHT-MARGIN* 20) (*print-shared* T))
   "(SETQ A (CAR F)
      B C ..)")
  ((print*c "(setq A #1=(car f) B C D #1#)"
	    (*print-lines* 1) (*print-right-margin* 20) (*print-shared* T))
   "(SETQ A (CAR F) ..)")

  ((format*c (formatter "~:<~:<~W ~W ~W ~W~:>~:>") "(#1=(1 a #1# 2 3 4))"
	     (*print-shared* T))
   "(#1=(1 A #1# 2))")
  ((format*c (formatter "~:<~:<~W ~W ~W ~W~:>~:>") "(#1=(1 a #1# 2 3 4))"
	     (*print-shared* nil))
   "(#1=(1 A #1# 2))")
  ((format*c (formatter "~:<~:<~W ~W ~W ~W~:>~:>") "((1 #1=(a) #1# 2 3 4))"
	     (*print-shared* T))
   "((1 #1=(A) #1# 2))")
  ((format*c (formatter "~:<~:<~W ~W ~W ~W~:>~:>") "((1 #1=(a) #1# 2 3 4))"
	     (*print-shared* nil))
   "((1 (A) (A) 2))")
  ((format*c (formatter "~:<~W ~W ~W ~W~:>") "#1=(1 a #1# 2 3 4)" (*print-shared* T))
   "#1=(1 A #1# 2)")
  ((format*c (formatter "~:<~W ~W ~W ~W~:>") "#1=(1 a #1# 2 3 4)" (*print-shared* nil))
   "#1=(1 A #1# 2)")
  ((format*c (formatter "~:<~a ~a ~W ~a~:>") "#1=(1 a #1# 2 3 4)" (*print-shared* T))
   "#1=(1 A #1# 2)")
  ((format*c (formatter "~:<~a ~a ~W ~a~:>") "#1=(1 a #1# 2 3 4)" (*print-shared* nil))
   "#1=(1 A #1# 2)")
  ((format*c (formatter "~:<~a ~a ~<~:> ~a~:>") "#1=(1 a #1# 2 3 4)" (*print-shared* T))
   "#1=(1 A #1# 2)")
  ((format*c (formatter "~:<~a ~a ~<~:> ~a~:>") "#1=(1 a #1# 2 3 4)" (*print-shared* nil))
   "#1=(1 A #1# 2)")
  ((format*c (formatter "~:<~W ~W ~W ~W~:>") "#1=(1 2 . #1#)" (*print-shared* T))
   "#1=(1 2 . #1#)")
  ((format*c (formatter "~:<~W ~W ~W ~W~:>") "#1=(1 2 . #1#)" (*print-shared* nil))
   "#1=(1 2 . #1#)")
  ((format*c (formatter "~:<~a ~a ~a ~a~:>") "#1=(1 2 . #1#)" (*print-shared* T))
   "#1=(1 2 . #1#)")
  ((format*c (formatter "~:<~a ~a ~a ~a~:>") "#1=(1 2 . #1#)" (*print-shared* nil))
   "#1=(1 2 . #1#)")
  ((format*c (formatter "~:<~a ~a ~a ~2@*~a ~a~:>") "#1=(1 2 3 4 5)" (*print-shared* T))
   "(1 2 3 3 4)")
  ((format*c (formatter "~:<~a ~a ~a ~2:*~a ~a~:>") "#1=(1 2 3 4 5)" (*print-shared* T))
   "(1 2 3 2 3)")

  ((format*c (formatter "~:<~W ~W ~:@<~W ~W~:>~:>") "#1=(1 a b . #1#)" (*print-shared* T))
   "#1=(1 A (B . #1#))")
  ((format*c (formatter "~@{~:<~W ~W ~:@<~W ~W~:>~:>~}") "#1=(1 a . #1#)" (*print-shared* T))
   "#1=(1 A #1#)")
  ((format*c (formatter "~:<~W ~W ~:@<~W ~W~:>~:>") "#1=(1 . #1#)" (*print-shared* T))
   "#1=(1 . #1#)")

;Some tests for particular problems that came up along the way

  ((plet 15 0 (xp::format nil (formatter "aaa~@<bbb~_ccc~:>ddd~_eee")))
   "aaabbbcccdddeee")
  ((plet 14 0 (xp::format nil (formatter "aaa~@<bbb~_ccc~:>ddd~_eee")))
   "aaabbbcccddd
eee")
  ((plet 12 0 (xp::format nil (formatter "aaa~@<bbb~_ccc~:>ddd~_eee")))
   "aaabbbcccddd
eee")
  ((plet 11 0 (xp::format nil (formatter "aaa~@<bbb~_ccc~:>ddd~_eee")))
   "aaabbb
   cccddd
eee")
  ((plet 5 0  (xp::format nil (formatter "aaa~@<bbb~_ccc~:>ddd~_eee")))
   "aaabbb
   cccddd
eee")

  ((plet 15 0 (xp::format nil (formatter "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
   "aaabbbcccdddeee")
  ((plet 14 0 (xp::format nil (formatter "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
   "aaabbbcccddd
 eee")
  ((plet 12 0 (xp::format nil (formatter "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
   "aaabbbcccddd
 eee")
  ((plet 11 0 (xp::format nil (formatter "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
   "aaabbb
   cccddd
 eee")
  ((plet 5 0  (xp::format nil (formatter "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
   "aaabbb
   cccddd
 eee")

   ((plet 15 0 (let ((*print-lines* 1))
		 (xp::format nil (formatter "~W") '(101 bar b zoto))))
    "(101 BAR B ..)")
   ((plet 15 0 (let ((*print-lines* 1))
		 (xp::format nil (formatter "~W") '(101 bar ba zoto))))
    "(101 BAR BA ..)")
   ((plet 15 0 (let ((*print-lines* 1))
		 (xp::format nil (formatter "~W") '(101 bar baz zoto))))
    "(101 BAR ..)")
   ((plet 15 0 (let ((*print-lines* 1))
		 (xp::format nil (formatter "~W") '(101 (20 2) zoto))))
    "(101 (20 2) ..)")
   ((plet 15 0 (let ((*print-lines* 1))
		 (xp::format nil (formatter "~W") '(101 (20 20) zoto))))
    "(101 ..)")
  ((plet 15 0
     (let ((*print-lines* 2))
       (xp::format nil (formatter "~:<---~<~;~a ~_~a ~_~a~;>+~:>--~:>") '((12 3456 789)))))
   "(---12
    3456 ..>+)")
   
;tests of obscure control paths.

   ((plet 20 0 (with-output-to-string (s)
		 (princ "abcde" s)
		 (xp::format s (formatter "~%~@<1234~:@_5678~:>"))))
    "abcde
1234
5678")

  ((plet 20 0 (xp::format nil (formatter "~@<foo ~4:ia~:@_b~:>")))
   "foo a
        b")
  ((plet 20 0 (xp::format nil (formatter "~@<foo ~4:ia~:@_~:@_b~:>")))
   "foo a

        b")

;tests for going over the length of various queues and stacks.
 #-franz-inc
  ((progn (setq xp::*free-xps* nil)
	  (plet 400 0 (xp::format nil (formatter "~@<foo ~300ia~:@_b~305i~:@_c~:>"))))
   #,(lisp:format nil "foo a~%~300@Tb~%~305@Tc"))

 #-franz-inc
  ((progn (setq xp::*free-xps* nil)
	  (plet 400 0
	    (xp::format nil (formatter "~@<foo ~250ia~:@_~@<0123456~@;b~@<++++~@;c~:@_d~:>~:>~:>"))))
   #,(lisp:format nil "foo a~%~250@T0123456b++++c~%~250@T0123456 ++++d"))

 #-franz-inc
  ((progn (setq xp::*free-xps* nil)
	  (plet 400 0 (xp::format nil (formatter "~@<~250@Ta~_~10@Tb~5@Tc~:>"))))
   #,(lisp:format nil "~250@Ta~10@Tb~5@Tc"))
 #-franz-inc
  ((progn (setq xp::*free-xps* nil)
	  (plet 200 0 (xp::format nil (formatter "~@<~250@Ta~10@Tb~5@Tc~:>"))))
   #,(lisp:format nil "~250@Ta~10@Tb~5@Tc"))

  ((progn (setq xp::*free-xps* nil)
	  (plet 85 0 (xp::format nil (formatter "~W") 
'((((((((((((((((((((((((((((((((((((((setq a b
					    c d)))))))))))))))))))))))))))))))))))))))))
   ;the next 2 lines must have spaces on them, not tabs
   "((((((((((((((((((((((((((((((((((((((SETQ A B
                                           C D))))))))))))))))))))))))))))))))))))))")

  ((progn (setq xp::*free-xps* nil)
	  (plet 200 0 (xp::format nil (formatter "~W") 
'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
   "(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)")
  ((progn (setq xp::*free-xps* nil)
	  (plet 50 0 (xp::format nil (formatter "~W") 
'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
   "(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 1 1 1 1 1 1 1 1)")

;testing error checking

  (etest (xp::format nil (formatter "ab~1,2")) (1 2))
  (etest (xp::format nil (formatter "ab~1,'")) (2 5))
  (etest (xp::format nil (formatter "ab~1/foo")) (3 4))
  (etest (xp::format nil (formatter "ab~1{foo~(..~{..~} ~}")) (4 9))
  (etest (xp::format nil (formatter "ab~!foo")) (5 3))
  (etest (xp::format nil (formatter "ab~1,2:@Ifoo")) (6 6))
  (etest (xp::format nil (formatter "ab~2:@:Ifoo")) (7 6))
  (etest (xp::format nil (formatter "ab~2:@@Ifoo")) (8 6))
  (etest (xp::format nil (formatter "ab~2:%foo")) (9 5))
  (etest (xp::format nil (formatter "ab~2@%foo")) (10 5))
  (etest (xp::format nil (formatter "ab~2@:[foo~]")) (11 6))
  (etest (xp::format nil (formatter "ab~:[foo~]")) (13 4))
  (etest (xp::format nil (formatter "ab~@[foo~;bar~]")) (14 4))
  (etest (xp::format nil (formatter "ab foo~;bar~]")) (15 7))
  (etest (xp::format nil (formatter "ab ~(foo~]bar~)")) (16 9))
  (etest (xp::format nil (formatter "ab ~[foo~)bar~]")) (17 9))
  (etest (xp::format nil (formatter "ab ~[foo~>bar~]")) (18 9))
  (etest (xp::format nil (formatter "ab ~[foo~}bar~]")) (19 9))
  (etest (xp::format nil (formatter "ab ~#<ff~>foo")) (21 4))
  (etest (xp::format nil (formatter "ab ~<f~#%f~>foo")) (21 7))
  (etest (xp::format nil (formatter "ab ~<f~#af~>foo")) (21 7))
  (etest (xp::format nil (formatter "ab ~22<f~:;f~>foo")) (22 10))
  (etest (xp::format nil (formatter "ab ~<f~Wf~>foo")) (23 7))
  (etest (xp::format nil (formatter "ab ~<f~;g~;h~;f~:>foo")) (24 4))
  (etest (xp::format nil (formatter "ab ~<f~Af~;g~;h~:>foo")) (25 5))
  (etest (xp::format nil (formatter "ab ~<f~;g~;h~Ag~:>foo")) (26 11))

;tests added in later releases.

   ((xp::format nil "A ~:<1 ~_2~:>~%B")
    "A (1 2)
B")
   ((ftest 40 0 '(list (loop for x in l and z in y do (print x) (print z) 
			      collect z into w)
			(print 111)))
    "(LIST (LOOP FOR X IN L
            AND Z IN Y
            DO (PRINT X)
               (PRINT Z)
            COLLECT Z INTO W)
      (PRINT 111))")
   ((ftest 40 0 '(loop (do this) and that and that
		       (print (list lots of stuff))))
    "(LOOP (DO THIS)
      AND
      THAT
      AND
      THAT
      (PRINT (LIST LOTS OF STUFF)))")
   ((ftest 40 0 '(loop for i in numbers-list
		       when (oddp i)
			 do (print i)
			 and collect i into odd-numbers
			 and do (terpri)
		       else
			 collect i into even-numbers
		       finally (return (values odd-numbers even-numbers))))
    "(LOOP FOR I IN NUMBERS-LIST
      WHEN (ODDP I)
        DO (PRINT I)
        AND COLLECT I INTO ODD-NUMBERS
        AND DO (TERPRI)
      ELSE
        COLLECT I INTO EVEN-NUMBERS
      FINALLY (RETURN (VALUES ODD-NUMBERS
                              EVEN-NUMBERS)))")
   ((ftest 60 0 '(loop for x in l and z in y do (print x) (print z)
		       when (plusp x) do (print x) end
		       when (plusp x) unless (plusp y) do (print x) and do (print y)
		                             else do (print z)
		       when (plusp x) unless (plusp y) do (print x) and do (print y) end
		       else do (print z)
		       when (plusp x) do (print x) 
		       else do (print y)
		       if (zerop y) do (print z) and do (print w)))
    "(LOOP FOR X IN L
      AND Z IN Y
      DO (PRINT X)
         (PRINT Z)
      WHEN (PLUSP X) DO (PRINT X) END
      WHEN (PLUSP X)
        UNLESS (PLUSP Y)
          DO (PRINT X)
          AND DO (PRINT Y)
        ELSE
          DO (PRINT Z)
      WHEN (PLUSP X)
        UNLESS (PLUSP Y)
          DO (PRINT X)
          AND DO (PRINT Y)
        END
      ELSE
        DO (PRINT Z)
      WHEN (PLUSP X) DO (PRINT X) ELSE DO (PRINT Y)
      IF (ZEROP Y)
        DO (PRINT Z)
        AND DO (PRINT W))")

#+(or :lucid symbolics)
  ((ftest 55 0 '`(COND (,A . B) ,C (D .,E) ,.F ,@G (H I)))
   "`(COND (,A . B) ,C (D .,E) ,.F ,@G (H I))")
#+(or :lucid symbolics)
  ((ftest 55 0 '`(WHEN (EQ ',I I)
		   (WHEN (CONSP ,M)
		     (DOLIST (C (IF (CONSP (CAR ,M)) ,@M `(,M)))
                       (PUSH `(',I .,J) ,C)))))
   "`(WHEN (EQ ',I I)
   (WHEN (CONSP ,M)
     (DOLIST (C (IF (CONSP (CAR ,M)) ,@M `(,M)))
       (PUSH `(',I .,J) ,C))))")

;tests of things that only work on Symbolics machines.

#+symbolics  ;the ~V~ gives many CLs fits.
  ((formats "~A-~10:<~V~ ~;~A~>-~A" 1 3 'bar 2))
#+symbolics
  ((plet 20 0 (xp::format nil (formatter "~|a~3|"))) #.(lisp:format nil "~|a~3|"))
#+symbolics
  (deftest
    (plet 100 0
      (let ((*print-level* 3))
	(defstruct tester2 a b)
	(xp::format nil (formatter "0~:@<1~W~:>") (make-tester2 :A '(1 (2 (3)))))))
   "0(1#S(TESTER2 :A (1 #) :B NIL))")
#+symbolics
  ((plet 100 0
     (let ((*print-pretty* t) x)
      (cons
       (with-output-to-string (s)
	(setq x (list
	 (xp::prin1 2 s)
	 (xp::finish-output s)
	 (xp::prin1 2 s)
	 (xp::force-output s)
	 (xp::prin1 2 s)
	 (xp::clear-output s))))
       x)))
   ("222" ;note this is very implementation dependent.
    2 nil 2 nil 2 nil))
#+symbolics
  ((print*c "#1=#2A((0 0 0) (0 (1 #2=(A A) #1#) #2#))" (*print-shared* T))
            "#1=#2A((0 0 0) (0 (1 #2=(A A) #1#) #2#))")
#+symbolics
  ((print*c "#1=#2A((0 0 0) (0 (1 #2=(A A) #1#) #2#))" (*print-shared* nil))
            "#1=#2A((0 0 0) (0 (1 (A A) #1#) (A A)))")

#+symbolics
  ((ftest 45 0 '(cons '(cons (member foo)) 
		  #'(lambda (xp list)
		      (xp::install xp (formatter "~W") list)) 0 *IPD*))
"(CONS '(CONS (MEMBER FOO))
      #'(LAMBDA (XP LIST)
          (XP::INSTALL XP #\"~W\" LIST))
      0
      *IPD*)")
#+symbolics
  ((ftest 55 0 '(zl:do-named foo ((a x (cdr a)) (i 1 (1+ i))) ((plusp a) T) (print a)))
   "(ZL:DO-NAMED FOO
             ((A X (CDR A)) (I 1 (1+ I)))
             ((PLUSP A) T)
  (PRINT A))")

))

;------------------------------------------------------------------------

;Copyright 1989,1990 by the Massachusetts Institute of Technology, Cambridge, 
;Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------
