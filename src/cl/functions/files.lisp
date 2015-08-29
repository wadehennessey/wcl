;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun delete-file (pathname)
  (let* ((filename (namestring pathname))
	 (status (unlink filename)))
    (if (= status 0)
	t
	(error "Cannot delete file ~A" filename))))

(defun enough-namestring (name)
  (namestring name))

(defmethod file-author ((stream file-stream))
  (warn "write FILE-AUTHOR")
  nil)

(defmethod file-length ((stream file-stream))
  (file_length (file-stream-input-source stream)))

(defun file-position (stream &optional position)
  (let ((fptr (or (stream-input-source stream)
		  (stream-output-sink stream))))
    (if (null position)
	(ftell fptr)
	(progn (case position
		 (:start (fseek fptr 0 0))
		 (:end (fseek fptr 0 2))
		 (t (if (and (fixnump position)
			     (<= position (file-length stream)))
			(fseek fptr position 0)
			(error "~D is not a legal file-position for ~A"
			       position stream))))
	       t))))

(defun file-write-date (filename)
  (let ((date (file_write_date (namestring filename))))
    (if (= date -1)
	nil
	(unix->universal-time date))))


(defun make-pathname (&key (host nil host?)
			   (device nil device?)
			   (directory nil directory?)
			   (name nil name?)
			   (type nil type?)
			   defaults)
  (let ((default-pathname (and defaults (pathname defaults))))
    (make-physical-pathname 
     :host (if host? host (and defaults (pathname-host defaults)))
     :device (if device? device (and defaults (pathname-device defaults)))
     :directory (if directory? directory (and defaults
					      (pathname-directory defaults)))
     :name (if name? name (and defaults (pathname-name defaults)))
     :type (if type? type (and defaults (pathname-type defaults))))))

(defun merge-pathnames (name &optional
			      (defaults *default-pathname-defaults*)
			      default-version)
  (let* ((primary (pathname name))
	 (secondary (pathname defaults))
	 (primary-dir (pathname-directory primary))
	 (secondary-dir (pathname-directory secondary))
	 (dir (cond ((null primary-dir) secondary-dir)
		    ((null secondary-dir) primary-dir)
		    (t (cond ((eq (car primary-dir) :relative)
			      (append secondary-dir (cdr primary-dir)))
			     ((eq (car primary-dir) :up)
			      (append (butlast secondary-dir)
				      (cdr primary-dir)))
			     (t primary-dir))))))
    (make-pathname :host nil
		   :device nil
		   :directory dir
		   :name (or (pathname-name primary)
			     (pathname-name secondary))
		   :type (or (pathname-type primary)
			     (pathname-type secondary)))))


(defun namestring (name)
  (etypecase name
    (physical-pathname (if (null (physical-pathname-namestring name))
			   (setf (physical-pathname-namestring name)
				 (pathname->string (pathname name)))
			   (physical-pathname-namestring name)))
    (string name)
    (stream (namestring (file-stream-pathname name)))
    (symbol (symbol-name name))))

(defun parse-namestring (namestring &optional host
				    (defaults *default-pathname-defaults*)
				    &key (start 0) end junk-allowed)
  (let* ((string (string namestring))
	 (last (or end (length string))))
    (values (parse-namestring-1 string last) last)))

(defun parse-namestring-1 (namestring end)
  (if (>= end 2)
      (let* ((dirlist
	      (let ((first-char (aref namestring 0))
		    (second-char (aref namestring 1)))
		(cond ((char= first-char #\/)
		       (cons :absolute (namestring->dirlist namestring 0)))
		      ((char= first-char #\~)
		       (cons :home (namestring->dirlist namestring 1)))
		      ((and (char= first-char #\.) (char= second-char #\/))
		       (cons :relative (namestring->dirlist namestring 2)))
		      ((and (char= first-char #\.) (char= second-char #\.))
		       (cons :up (namestring->dirlist namestring 2)))
		      (t (let ((l (namestring->dirlist namestring -1)))
			   (if (null l)
			       l
			       (cons :relative l)))))))
	     (last-slash (position #\/ namestring :from-end t))
	     (filename-start (if (null last-slash) 0 (1+ last-slash)))
	     (dot (position #\. namestring :start filename-start))
	     (type-start (if (null dot) end (1+ dot))))
	(make-pathname
	 :namestring namestring
	 :host nil
	 :device nil
	 :directory dirlist
	 :name (null-string->nil
		(subseq namestring
			filename-start
			(or dot end)))
	 :type (null-string->nil
		(subseq namestring type-start end))))
      (make-pathname :namestring namestring
		     :name (null-string->nil namestring)
		     :directory (if (and (= end 1)
					 (char= (aref namestring 0) #\.))
				    (list :relative)
				    nil))))

(defun null-string->nil (string)
  (if (= (length string) 0) nil string))

(defun namestring->dirlist (namestring last)
  (let ((next (position #\/  namestring :start (1+ last))))
    (if (null next)                                                        
	nil
	(cons (subseq namestring (1+ last) next)
	      (namestring->dirlist namestring next)))))


(defun pathname-device (p)
  (physical-pathname-device (pathname p)))

(defun pathname-directory (p)
  (physical-pathname-directory (pathname p)))

(defun pathname-host (p)
  (physical-pathname-host (pathname p)))

(defun pathname-name (p)
  (physical-pathname-name (pathname p)))

(defun nil->empty-string (x)
  (if (null x)
      ""
      x))

(defun pathname->string (p)
  (let ((dirlist (pathname-directory p)))
    (format nil "~A~{~A/~}~A~A~A"
	    (case (car dirlist)
	      (:absolute "/")
	      (:relative "./")
	      (:up "../")
	      (t ""))
	    (cdr dirlist)
	    (nil->empty-string (pathname-name p))
	    (if (null (pathname-type p)) "" ".")
	    (nil->empty-string (pathname-type p)))))

(defun pathname-type (p)
  (physical-pathname-type (pathname p)))

(defun pathname-version (p)
  (physical-pathname-version (pathname p)))


(defun pathname (x)
  (etypecase x
    (string (values (parse-namestring x)))
    (pathname x)
    (stream (file-stream-pathname x))))

(defun probe-file (pathname)
  (let ((truename (probe_file (namestring pathname))))
    (if (stringp truename)
	(pathname truename)
	nil)))

(defun truename (pathname)
  (let ((truename (probe_file (namestring pathname))))
    (if (= truename 0)
	(error "The file ~S does not exist" pathname)
	truename)))

(defun directory (dir)
  (let* ((path (pathname dir))
	 (name (namestring path))
	 (dirp (opendir name)))
    (loop for dirent = (readdir dirp)
	  until (eq dirent 0)
	  collect (merge-pathnames (dirent-name dirent) path)
	  finally (closedir dirp))))

(defun ed (&optional pathname)
  (declare (ignore pathname))
  (warn "No editor available"))

(defun file-namestring (file)
  (let ((pathname (pathname file)))
    (concatenate 'string (pathname-name file) "." (pathname-type file))))

(defun directory-namestring (file)
  (namestring (make-pathname :directory (pathname-directory file))))

(defun host-namestring (file)
  "")

