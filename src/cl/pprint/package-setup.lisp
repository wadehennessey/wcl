;;; Copyright 1989,1990 by the Massachusetts Institute of Technology,
;;; Cambridge, Massachusetts.

;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted,
;;; provided that this copyright and permission notice appear in all
;;; copies and supporting documentation, and that the name of M.I.T. not
;;; be used in advertising or publicity pertaining to distribution of the
;;; software without specific, written prior permission. M.I.T. makes no
;;; representations about the suitability of this software for any
;;; purpose.  It is provided "as is" without express or implied warranty.
;;;
;;;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;    SOFTWARE.


;;; This file "XP.LISP" contains the 8/31/90 implemetation of an
;;; efficient pretty printer for Common Lisp.  The functions in this file
;;; are documented in Chapter 27 of
;;;   Common Lisp: the Language Second Edition,
;;;   Guy L. Steele Jr, Digital press, 1990,
;;; and in even greater detail in
;;;   MIT/AIM-1102a, July 1989.  
;;; This report can be obtained by writing to
;;;
;;;             Publications
;;; 	       MIT AI Laboratory
;;; 	       545 Tech. Sq.
;;; 	       Cambridge MA 02139
;;;
;;; The companion file "XPTEST.LISP" contains a set of 600+ tests.  You should
;;; run these tests after the first time you compile this file on a new system.
;;;
;;; The companion file "XPDOC.TXT" contains brief documentation.

(in-package "XP" :use '("LISP"))

(shadow '(write print prin1 princ pprint format write-to-string princ-to-string
	  prin1-to-string write-line write-string write-char terpri fresh-line
	  defstruct finish-output force-output clear-output))

(export '(formatter copy-pprint-dispatch pprint-dispatch
	  set-pprint-dispatch pprint-fill pprint-linear pprint-tabular
	  pprint-logical-block pprint-pop pprint-exit-if-list-exhausted
	  pprint-newline pprint-indent pprint-tab 
	  *print-pprint-dispatch* *print-right-margin* *default-right-margin*
	  *print-miser-width* *print-lines*
	  *last-abbreviated-printing*
	  *print-shared*))
	  

