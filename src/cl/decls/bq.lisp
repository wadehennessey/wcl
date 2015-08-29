;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defvar *comma* "COMMA")
(defvar *comma-atsign* "COMMA-ATSIGN")
(defvar *comma-dot*  "COMMA-DOT")
(defvar *bq-list* "BQ-LIST")
(defvar *bq-append* "BQ-APPEND")
(defvar *bq-list** "BQ-LIST*")
(defvar *bq-nconc* "BQ-NCONC")

(defvar *bq-clobberable* "BQ-CLOBBERABLE")
(defvar *bq-quote* "BQ-QUOTE")
(defvar *bq-quote-nil* (list  *bq-quote* nil))

(defparameter *bq-simplify* t)

(defvar *bq-count* 0)

(defmacro backquote (x)
  (bq-completely-process x))
