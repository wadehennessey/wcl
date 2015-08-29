;;;-*-Mode:LISP; Package:(PCL (LISP WALKER)); Base:10; Syntax:Common-lisp -*-
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

(in-package 'pcl)

;;; These redefine the macros in low.lisp which contain (locally ...).
;;; I don't think you can do (setf (locally ...) v)
(defmacro %svref (vector index)
  `(svref (the simple-vector ,vector) (the fixnum ,index)))

(defsetf %svref (vector index) (new-value)
  `(setf (svref (the simple-vector ,vector) (the fixnum ,index))
    ,new-value))

(defun printing-random-thing-internal (thing stream)
  (write (lisp::object->pointer thing) :stream stream :base 16))

