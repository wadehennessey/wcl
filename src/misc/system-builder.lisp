;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

;;; Wimpy system building tool. Not Fancy and mildly useful.

(in-package "SYSTEM-BUILDER"
	    :nicknames '("SB")
	    :use '("LISP" #+LUCID "LUCID-COMMON-LISP"))

;;; Desired improvements :
;;;   compile and load source
;;;   save-system
;;;   :recompile-all
;;;   :load-only
;;;   :bin-directory

(export '(define-system
          build-system
	  system-files
	  count-system-lines
	  count-system-characters
          edit-system
          print-system))

(defvar *verbose-build* nil "True for verbosity")
(defvar *system-builder-compiler*)
(defvar *skip-load?*)
(defvar *force-update?*)

(defstruct system-info
  name
  package
  files
  directory
  source-extension
  binary-extension
  load-date-table)

(defvar *info*
  nil
  "Current SYSTEM-INFO structure for system being worked on")

(defvar *known-systems*
  (make-hash-table)
  "Information about all systems defined with DEFINE-SYSTEM")

(defmacro sys-dependent-defer-warnings (&rest forms)
  #+lucid `(with-deferred-warnings ,@forms)
  #+cmu `(with-compilation-unit () ,@forms)
  #-lucid `(progn ,@forms))

(defmacro define-system (name &rest options)
  `(define-system-internal ',name ,@options))

(defun find-system (name)
  (gethash name *known-systems*))

(defun define-system-internal (name &key
				    (package name)
				    files
				    directory
				    source-extension
				    binary-extension)
  (labels ((update-system-info (info)
	     (setf (system-info-name info) name)
	     (setf (system-info-package info) package)
             (setf (system-info-files info) files)
	     (setf (system-info-directory info) directory)
	     (setf (system-info-source-extension info)
		   source-extension)
	     (setf (system-info-binary-extension info)
		   binary-extension)
             (when (null (system-info-load-date-table info))
	       (setf (system-info-load-date-table info)
		     (make-hash-table :test #'equal)))))
    (let ((info (gethash name *known-systems*)))
      (if (null info)
	  (let ((new-info (make-system-info :name name)))
	    (update-system-info new-info)
	    (setf (gethash name *known-systems*) new-info))
	  (update-system-info info))
      name)))

;;; BUILD means compile-load "as needed".
;;; Lots of stuff hidden in "as needed."
(defun perform-system-action (name action-function)
  (let ((*info* (find-system name)))
    (if (null *info*)
	(error "No information found for system ~A." name)
	(let ((*package* (find-package (system-info-package *info*))))
	  (funcall action-function)))))

(defun build-system (name &key ((:compile-file *system-builder-compiler*)
				#'compile-file)
			  ((:skip-load? *skip-load?*) nil)
			  ((:force-update? *force-update?*) nil))
  (sys-dependent-defer-warnings
   (perform-system-action name #'build-system-internal)))

(defun system-files (name)
  (perform-system-action name #'list-current-system-files))

(defun count-system-lines (name)
  (perform-system-action name #'count-system-lines-internal))

(defun count-system-characters (name)
  (perform-system-action name #'count-system-characters-internal))

(defun edit-system (name)
  (perform-system-action name #'edit-system-internal))

(defun print-system (name)
  (perform-system-action name #'print-system-internal))

(defun build-system-internal ()
  (let ((*load-verbose* nil)
	(component (system-info-files *info*)))
    (build-component component :if-needed)))

(defun build-components (components condition)
  (dolist (component components)
    (build-component component condition)))

(defun build-component (component condition)
  (if (atom component)
      (update-and-load-file component condition)
      (funcall (case (first component)
		 (:sequential #'sequential-build)
		 (:parallel #'parallel-build)
		 (:interpreted-only #'interpreted-build)
		 (t #'sequential-build))
	       (cdr component)
	       condition)))

;;; Build-the sub-components sequentially, updating branch-need; however,
;;; as soon as sub-component N requires updating, then all subsequent
;;; sub-components MUST be rebuilt.
(defun sequential-build (components condition)
  (labels ((iter (components must-update?)
	     (if (null components)
		 must-update?
		 (iter (cdr components)
		       (build-component (car components)
					(if must-update?
					    :always
					    :if-needed))))))
    (iter components (if (eq condition :always)
			 t
			 nil))))

(defun parallel-build (components condition)
  (do ((rest (cdr components) (cdr rest))
       (component (car components) (car rest))
       (any-updates? nil (let ((update? (build-component component
							 condition)))
			   (or update? any-updates?))))
      ((null component) any-updates?)))

(defun interpreted-build (component)
  (declare (ignore component))
  (error "Not written yet"))

;;; Return T if the component needs updating, else NIL.
(defun update-and-load-file (file condition)
  (prog1 (update-file file condition)
    (unless *skip-load?*
      (load-file (make-full-pathname file :binary) condition))))

(defun update-file (file condition)
  (if (eq condition :always)
      (do-compile-file file "~%:SEQUENTIAL update ~S")
      (if (file-update-needed-p file)
	  (do-compile-file file "~%~S needs updating.")
	  nil)))

;;; Always return true to indicate that we updated the file.
(defun do-compile-file (file format-string)
  (let ((source-name (make-full-pathname file :source)))
    (format t format-string source-name source-name)
    (force-output t)
    (funcall *system-builder-compiler* source-name)
    t))

(defun file-update-needed-p (file)
  (or *force-update?*
      (let ((source-name (make-full-pathname file :source))
	    (binary-name (make-full-pathname file :binary)))
	(or (null (probe-file binary-name))
	    (> (file-write-date source-name)
	       (file-write-date binary-name))))))

(defun load-file (pathname condition)
  (let* ((current-load-date-table (system-info-load-date-table *info*))
	 (last-load-write-date (gethash pathname current-load-date-table))
	 (bin-pathname (make-full-pathname pathname :binary))
	 (current-write-date (if (probe-file bin-pathname)
				 (file-write-date bin-pathname)
				 nil))
	 (reload-needed? (or (null last-load-write-date)
			     (null current-write-date)
			     (not (= last-load-write-date
				     current-write-date)))))
    (if (or (eq condition :always)
	    reload-needed?)
	(progn (if (and (eq condition :always)
			(not reload-needed?))
		   (format t
			   "~%:always reload of ~S..."
			   pathname)
		   (format t "~&Loading ~S... " pathname))
	       (force-output *terminal-io*)
	       (load bin-pathname)
	       (setf (gethash pathname current-load-date-table)
		     current-write-date)
	       (format t "done.~%"))
	(when *verbose-build*
	  (format t "~%~S has not changed since last load" pathname)))))

(defun make-full-pathname (file type)
  (merge-pathnames
   file
   (make-pathname :host (pathname-host
			 (system-info-directory *info*))
		  #+cmu :device #+cmu :absolute
		  :directory (pathname-directory (system-info-directory
						  *info*))
		  :type (ecase type
			  (:source (system-info-source-extension
				    *info*))
			  (:binary (system-info-binary-extension
				    *info*))))))


;;; Useful stuff for counting source code lines.
(defun count-system-lines-internal ()
  (let ((files (list-current-system-files)))
    (do ((file-list (cdr files) (cdr file-list))
	 (file (car files) (car file-list))
	 (sum 0 (progn
		 (format t "~%Counting lines in ~A..." file)
		 (let ((num-lines (count-file-lines
				   (make-full-pathname file :source))))
		   (format t " read ~A lines." num-lines)
		   (+ sum num-lines)))))
	((null file) (format t
			     "~%Total of ~A lines in system ~A"
			     sum
			     (system-info-name *info*))
	             sum))))

(defun count-system-characters-internal ()
  (let ((files (list-current-system-files)))
    (do ((file-list (cdr files) (cdr file-list))
	 (file (car files) (car file-list))
	 (sum 0 (progn
		 (format t "~%Counting characters in ~A..." file)
		 (let ((num-chars (count-file-characters
				   (make-full-pathname file :source))))
		   (format t " read ~A characters" num-chars)
		   (+ sum num-chars)))))
	((null file) (format t
			     "~%Total of ~A characters in system ~A"
			     sum
			     (system-info-name *info*))
	             sum))))

;;; This is ugly and should be rewritten
(defun list-current-system-files ()
  (labels ((list-em (rest so-far)
	     (if (null rest)
		 (nreverse so-far)
		 (list-em (cdr rest)
			  (if (listp (car rest))
			      (append (nreverse (list-em (car rest) nil))
				      so-far)
			      (if (member (car rest)
					  '(:serial :parallel))
				  so-far
				  (cons (car rest) so-far)))))))
    (let ((files (list-em (system-info-files *info*) nil)))
      (loop for f in files collect (make-full-pathname f :source)))))

(defun count-file-lines (pathname)
  (with-open-file (file-stream pathname)
    (do ((count 0 (1+ count))
	 (line (read-line file-stream nil file-stream)
	       (read-line file-stream nil file-stream)))
	((eq line file-stream) count))))

(defun count-file-characters (pathname)
  (with-open-file (file-stream pathname)
    (do ((count 0 (1+ count))
	 (char (read-char file-stream nil file-stream)
	       (read-char file-stream nil file-stream)))
	((eq char file-stream) count))))

