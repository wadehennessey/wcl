;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(defun get-string-buffer ()
  (if (null *free-string-buffers*)
      (make-string default-string-buffer-length)
      (pop *free-string-buffers*)))

(defun-inline free-string-buffer-1 (buffer)
  (push buffer *free-string-buffers*))

(defun free-string-buffer (buffer length)
  (let ((new (make-string length)))
    (loop for i from 0 below length
	  do (setf (schar new i) (schar buffer i)))
    (free-string-buffer-1 buffer)
    new))

(defun string-buffer->simple-string (buffer)
  (loop with len = (length buffer)
	with result = (make-string len)
	for i from 0 below len do
	(setf (schar result i) (aref buffer i))
	finally (return result)))

(defun get-string-stream ()
  (if (null *free-string-streams*)
      (make-string-stream)
      (pop *free-string-streams*)))

(defun free-string-stream (stream)
  (push stream *free-string-streams*))

(defun open (pathname &key (direction :input) (element-type 'character))
  (let* ((path (pathname pathname))
	 (filename (namestring path))
	 (access-method (ecase direction
			  (:output "w")
			  (:input "r")
			  (:io "rw")))
	 (fptr (fopen filename access-method)))
    (multiple-value-bind (element-type-tag element-size)
	(type->element-type-tag element-type)
      (if (legal-stream-element-type? element-type-tag)
	  (if (eql fptr 0)
	      (error "Cannot open file ~S" pathname)
	      (let ((input? (memq direction '(:input :io)))
		    (output? (memq direction '(:output :io))))
		(initialize-fptr-stream-methods
		 (if (and input? *input-stream-line-numbers?*)
		     (make-line-number-stream
		      :pathname path
		      :input-source (and input? fptr)
		      :output-sink (and output? fptr)
		      :element-type element-type-tag
		      :element-size element-size)
		     (make-file-stream
		      :pathname path
		      :input-source (and input? fptr)
		      :output-sink (and output? fptr)
		      :element-type element-type-tag
		      :element-size element-size))
		 input?
		 output?)))
	  (error "~A is not a legal stream element type" element-type)))))

(defun open-user-stream (&key input-fd
			      output-fd
			      (element-type 'character))
  (multiple-value-bind (element-type-tag element-size)
      (type->element-type-tag element-type)
    (if (legal-stream-element-type? element-type-tag)
	(let ((input? (not (null input-fd)))
	      (output? (not (null output-fd))))
	  (initialize-fptr-stream-methods
	   (make-user-stream
	    :input-source (if input? (fdopen input-fd "r") nil)
	    :output-sink (if output? (fdopen output-fd "w") nil)
	    :element-type element-type-tag
	    :element-size element-size)
	   input?
	   output?))
    	(error "~A is not a legal stream element type" element-type))))

(defun initialize-fptr-stream-methods (stream input? output?)
  (setf (stream-write-char-method stream)
	(if output?
	    #'write-char/fptr-stream
	    #'illegal-stream-operation))
  (setf (stream-write-byte-method stream)
	(if output?
	    #'write-byte/fptr-stream
	    #'illegal-stream-operation))
  (setf (stream-write-float-method stream)
	(if output?
	    #'write-float/fptr-stream
	    #'illegal-stream-operation))
  (setf (stream-write-simple-string-method stream)
	(if output?
	    #'write-simple-string/fptr-stream
	    #'illegal-stream-operation))
  (setf (stream-force-output-method stream)
	(if output?
	    #'force-output/fptr-stream
	    #'illegal-stream-operation))
  (setf (stream-read-char-method stream)
	(if input?
	    (if (line-number-stream-p stream)
		#'read-char/line-number-stream
		#'read-char/fptr-stream)
	    #'illegal-stream-operation))
  (setf (stream-read-byte-method stream)
	(if input?
	    #'read-byte/fptr-stream
	    #'illegal-stream-operation))
  (setf (stream-peek-char-method stream)
	(if input?
	    #'peek-char/fptr-stream
	    #'illegal-stream-operation))
  (setf (stream-unread-char-method stream)
	(if input?
	    #'unread-char/fptr-stream
	    #'illegal-stream-operation))
  (setf (stream-listen-method stream)
	(if input?
	    #'listen/fptr-stream
	    #'illegal-stream-operation))
  stream)

(defun open-terminal-stream (input-fptr output-fptr)
  (make-terminal-stream
   :input-source input-fptr
   :output-sink output-fptr
   :element-type element-type-char
   :element-size 8
   :write-char-method #'write-char/terminal-stream
   :read-char-method #'read-char/fptr-stream
   :write-byte-method #'write-byte/terminal-stream
   :read-byte-method #'read-byte/terminal-stream
   :write-float-method #'write-float/terminal-stream
   :write-simple-string-method #'write-simple-string/terminal-stream
   :force-output-method #'force-output/fptr-stream
   :peek-char-method #'peek-char/fptr-stream
   :unread-char-method #'unread-char/fptr-stream
   :listen-method #'listen/fptr-stream))

(defun make-string-output-stream (&optional string)
  (if (null string)
      (make-string-output-stream-1 (get-string-buffer) t)
      (make-string-output-stream-1 string nil)))

(defun make-string-output-stream-1 (buffer internal-buffer?)
  (make-string-stream
   :input-source nil
   :internal-buffer? internal-buffer?
   :output-sink buffer
   :element-type element-type-char
   :next 0
   :end (length buffer)
   :write-char-method #'write-char/string-stream
   :read-char-method #'illegal-stream-operation
   :write-byte-method #'illegal-stream-operation
   :read-byte-method #'illegal-stream-operation
   :write-float-method #'write-float/string-stream
   :write-simple-string-method #'write-simple-string/string-stream
   :force-output-method #'force-output/string-stream
   :peek-char-method #'illegal-stream-operation
   :unread-char-method #'illegal-stream-operation
   :listen-method #'illegal-stream-operation))

(defun make-string-input-stream (string &optional
					(start 0)
					(end (length string)))
  (make-string-stream
   :input-source string
   :internal-buffer? nil
   :output-sink nil
   :element-type element-type-char
   :next start
   :end end
   :write-char-method #'illegal-stream-operation
   :read-char-method #'read-char/string-stream
   :write-byte-method #'illegal-stream-operation
   :read-byte-method #'illegal-stream-operation
   :write-float-method #'illegal-stream-operation
   :write-simple-string-method #'illegal-stream-operation
   :force-output-method #'illegal-stream-operation
   :peek-char-method #'peek-char/string-stream
   :unread-char-method #'unread-char/string-stream
   :listen-method #'listen/string-stream))


(defun close (stream &key abort)
  (setf (stream-write-char-method stream) #'closed-stream-method)
  (setf (stream-read-char-method stream) #'closed-stream-method)
  (setf (stream-write-byte-method stream) #'closed-stream-method)
  (setf (stream-read-byte-method stream) #'closed-stream-method)
  (setf (stream-write-float-method stream) #'closed-stream-method)
  (setf (stream-write-simple-string-method stream) #'closed-stream-method)
  (setf (stream-force-output-method stream) #'closed-stream-method)
  (setf (stream-peek-char-method stream) #'closed-stream-method)
  (setf (stream-unread-char-method stream) #'closed-stream-method)
  (setf (stream-listen-method stream) #'closed-stream-method)
  (let ((input (stream-input-source stream))
	(output (stream-output-sink stream)))
    (typecase stream
      (fptr-stream
       (unless (null input) (fclose input))
       (unless (null output) (fclose output)))
      (string-stream
       (when (and (string-stream-internal-buffer? stream)
		  (not (null output)))
	 (free-string-buffer-1 output)))))
  nil)

(defun input-stream-p (stream)
  (not (null (stream-input-source stream))))

(defun output-stream-p (stream)
  (not (null (stream-output-sink stream))))

(defun illegal-stream-operation (stream &rest args)
  (error "Illegal operation attempted on stream ~A" stream))

(defun closed-stream-method (stream &rest args)
  (error "The stream ~A is closed for file operations" stream))

(defun-inline read-char-unsafe (stream eof-error-p eof-value)
  (funcall (the compiled-function
		(stream-read-char-method stream))
	   stream eof-error-p eof-value))

(defun read-char/3 (stream eof-error-p eof-value)
  (with-valid-stream (s stream)
    (read-char-unsafe s eof-error-p eof-value)))

(defun-inline read-char (&optional (stream *standard-input*)
				    eof-error-p eof-value recursivep)
  (read-char/3 stream eof-error-p eof-value))

(defun read-char/fptr-stream (stream eof-error-p eof-value)
  (let ((x (fgetc (file-stream-input-source stream))))
    (if (= x unix-eof-flag)
	(if eof-error-p
	    (error-stream-eof stream)
	    eof-value)
	(code-char x))))
				     
(defun read-char/line-number-stream (stream eof-error-p eof-value)
  (let ((char (read-char/fptr-stream stream eof-error-p eof-value)))
    (when (char= char #\Newline)
      (incf (line-number-stream-line stream)))
    char))

(defun read-char/string-stream (stream eof-error-p eof-value)
  (let ((next (string-stream-next stream)))
    (if (= next (string-stream-end stream))
	(if eof-error-p
	    (error-stream-eof stream)
	    eof-value)
	(prog1 (aref (stream-input-source stream) next)
	  (incf (string-stream-next stream))))))

(defun peek-char-unsafe (peek-type stream eof-error-p eof-value)
  (if (null peek-type)
      (funcall (the compiled-function
		    (stream-peek-char-method stream))
	       stream eof-error-p eof-value)
      (error "Add more peek-types to peek-char")))

(defun peek-char/4 (peek-type stream eof-error-p eof-value)
  (with-valid-stream (s stream)
    (peek-char-unsafe peek-type s eof-error-p eof-value)))

(defun-inline peek-char (&optional peek-type (stream *standard-input*)
				    eof-error-p eof-value recursivep)
  (peek-char/4 peek-type stream eof-error-p eof-value))

(defun peek-char/fptr-stream (stream eof-error-p eof-value)
  (let ((c (read-char stream eof-error-p eof-value)))
    (unread-char c stream)
    c))

(defun peek-char/string-stream (stream eof-error-p eof-value)
  (let ((next (string-stream-next stream)))
    (if (= next (string-stream-end stream))
	(if eof-error-p
	    (error-stream-eof stream)
	    eof-value)
	(aref (stream-input-source stream) next))))

(defun-inline write-char-unsafe (char stream)
  (funcall (the compiled-function
		(stream-write-char-method stream))
	   stream char))

(defun write-char/2 (char stream)
  (with-valid-stream (s stream)
    (write-char-unsafe char s)))

(defun-inline write-char (char &optional (stream *standard-output*))
  (write-char/2 char stream))

(defun write-character (x stream)
  (if (null *print-escape*)
      (write-char x stream)
      (write-character-escape x stream)))

(defun write-character-escape (char stream)
  (write-string "#\\" stream)
  (let ((name (char-name char)))
    (if (null name)
	(write-char char stream)
	(write-string name stream)))
  char)

(defun write-char/fptr-stream (stream char)
  (fputc char (stream-output-sink stream))
  char)

(defun write-char/terminal-stream (stream char)
  (write-char/fptr-stream stream char)
  (if (char= char #\Newline)
      (setf (terminal-stream-column stream) 0)
      (incf (terminal-stream-column stream)))
  char)

(defun write-char/string-stream (stream char)
  (let* ((next (string-stream-next stream))
	 (current-buffer (string-stream-output-sink stream))
	 (buffer (if (= next (string-stream-end stream))
		     (let* ((new-len (* 2 (length current-buffer)))
			    (new-buffer (make-string new-len)))
		       (replace new-buffer current-buffer)
		       (setf (string-stream-output-sink stream) new-buffer)
		       (setf (string-stream-end stream) new-len)
		       new-buffer)
		     current-buffer)))
    (setf (aref buffer next) char)
    (incf (string-stream-next stream))
    char))

(defun write-string-escape (string stream)
  (write-char #\" stream)
  (loop for i from 0 below (length string)
	do (let ((char (aref string i)))
	      (when (or (char= char #\\) (char= char #\"))
		(write-char #\\ stream))
	      (write-char char stream)))
  (write-char #\" stream)
  string)

(defun write-string/4 (string stream start end)
  (with-valid-stream (s stream)
    (write-string-unsafe string s start end)))

(defun write-line/4 (string stream start end)
  (with-valid-stream (s stream)
    (write-string-unsafe string s start end)
    (write-char #\Newline s)
    string))

(defun-inline write-simple-string-unsafe (string stream)
  (funcall (the compiled-function
		(stream-write-simple-string-method stream))
	   stream string))

(defun write-string-unsafe (string stream start end)
  (if (and (null start)
	   (null end)
	   (simple-string-p string))
      (write-simple-string-unsafe string stream)
      (write-any-string string stream start end)))

(defun-inline write-float-unsafe (f stream)
  (funcall (the compiled-function
		(stream-write-float-method stream))
	   stream f))

(defun write-float/2 (f stream)
  (with-valid-stream (s stream)
    (write-float-unsafe f s)))

(defun write-float (f stream)
  (write-float/2 f stream))

(defun write-float/fptr-stream (stream f)
  (print_double_float f (fptr-stream-output-sink stream))
  f)

(defun write-float/terminal-stream (stream f)
  (let ((size (print_double_float f (fptr-stream-output-sink stream))))
    (incf (terminal-stream-column stream) size))
  f)

(defun write-float/string-stream (stream f)
  (let* ((buflen 64)
	 (buffer (make-string buflen))
	 (len (double_float_to_string f buffer)))
    (if (>= len buflen)
	(error "Double float string printer error")
	(loop for i from 0 below buflen
	      do (let ((c (aref buffer i)))
		   (if (char= c #\Null)
		       (loop-finish)
		       (write-char c stream))))))
  f)

(defun write-any-string (string stream start end)
  (loop for i from (or start 0) below (or end (length string))
	do (write-char (aref string i) stream))
  string)

(defun write-simple-string/fptr-stream (stream string)
  (fputs string (stream-output-sink stream))
  string)

(defun write-simple-string/terminal-stream (stream string)
  (write-simple-string/fptr-stream stream string)
  (setf (terminal-stream-column stream)
	(string_column string (terminal-stream-column stream)))
  string)

(defun write-simple-string/string-stream (stream string)
  (write-any-string string stream 0 (length string)))

(defun-inline read-byte-unsafe (stream eof-error-p eof-value)
  (funcall (the compiled-function
		(stream-read-byte-method stream))
	   stream eof-error-p eof-value))  

(defun read-byte/3 (stream eof-error-p eof-value)
  (if (streamp stream)
      (read-byte-unsafe stream eof-error-p eof-value)
      (error "Cannot read bytes from ~A" stream)))

(defun-inline read-byte (stream &optional eof-error-p eof-value)
  (read-byte/3 stream eof-error-p eof-value))

(defun read-byte/fptr-stream (stream eof-error-p eof-value)
  (read_byte (fptr-stream-input-source stream)
	     (fptr-stream-element-type stream)
	     eof-error-p
	     eof-value))

(defun-inline write-byte-unsafe (integer stream)
  (funcall (the compiled-function
		(stream-write-byte-method stream))
	   stream
	   integer))

(defun write-byte (integer stream)
  (if (streamp stream)
      (write-byte-unsafe integer stream)
      (error "Cannot write bytes to ~A" stream)))

(defun write-byte/fptr-stream (stream integer)
  (write_byte integer
	      (fptr-stream-output-sink stream)
	      (fptr-stream-element-type stream))
  integer)

(defun read-vector (stream vector start end eof-error-p eof-value)
  (read_vector (fptr-stream-input-source stream)
	       (fptr-stream-element-size stream)
	       vector
	       start
	       end
	       eof-error-p
	       eof-value))

(defun write-vector (stream vector start end)
  (write_vector (fptr-stream-output-sink stream)
		(fptr-stream-element-size stream)
		vector
		start
		end))

(defun unread-char-unsafe (char stream)
  (funcall (the compiled-function
		(stream-unread-char-method stream))
	   stream char))

(defun unread-char/2 (char stream)
  (with-valid-stream (s stream)
    (unread-char-unsafe char s)))

(defun-inline unread-char (char &optional (stream *standard-input*))
  (unread-char/2 char stream))

(defun unread-char/fptr-stream (stream char)
  (ungetc char (fptr-stream-input-source stream))
  char)

(defun unread-char/string-stream (stream char)
  (decf (string-stream-next stream))
  char)

(defun-inline force-output (&optional (stream *standard-output*))
  (force-output/1 stream))

(defun force-output/1 (stream)
  (with-valid-stream (s stream)
    (force-output-unsafe s)))

(defun force-output-unsafe (s)
  (funcall (the compiled-function
		(stream-force-output-method s))
	   s))

(defun-inline finish-output (&optional (stream *standard-output*))
  (force-output stream))

(defun force-output/fptr-stream (stream)
  (fflush (stream-output-sink stream))
  nil)

(defun force-output/string-stream (stream)
  nil)

(defun clear-output (stream)
  nil)

(defun clear-input (&optional stream)
  nil)

(defun-inline listen-unsafe (stream)
  (funcall (the compiled-function
		(stream-listen-method stream))
	   stream))

(defun listen/1 (stream)
  (with-valid-stream (s stream)
    (listen-unsafe s)))

(defun-inline listen (&optional (stream *standard-input*))
  (listen/1 stream))

(defun listen/fptr-stream (stream)
  (not (= 0 (file_listen (fptr-stream-input-source stream)))))

(defun listen/string-stream (stream)
  (error "write listen for string stream"))

(defun stream-column (stream)
  (if (terminal-stream-p stream)
      (terminal-stream-column stream)
      0))

(defun write-integer (integer stream)
  (if (< integer 0)
      (progn (write-char #\- stream)
	     (write-integer-1 (abs integer) stream))
      (write-integer-1 integer stream))
  integer)

(defun write-integer-1 (integer stream)
  (multiple-value-bind (quotient remainder)
      (truncate integer *print-base*)
    (unless (= quotient 0)
      (write-integer-1 quotient stream))
    (write-char (code-char (if (> remainder 9)
			       (+ (char-code #\A) (- remainder 10))
			       (+ (char-code #\0) remainder)))
		stream)))

(defun legal-stream-element-type? (element-type)
  (member element-type
	  (list element-type-signed-8bit
		element-type-unsigned-8bit
		element-type-char
		element-type-signed-16bit
		element-type-unsigned-16bit
		element-type-signed-32bit
		element-type-unsigned-32bit)))

(defun error-stream-eof (stream)
  (error "Unexpected end of file reached on ~A" stream))

(defun initialize-streams ()
  (setf *terminal-io* (open-terminal-stream (get_file_ptr 0)
					    (get_file_ptr 1)))
  (setf *standard-input* *terminal-io*)
  (setf *standard-output* *terminal-io*)
  ;; We don't use stderr because of buffer problems that cause
  ;; stdout and stderr output to come out in the wrong order.
  (setf *error-output* *standard-output*)
  (setf *query-io* *standard-output*)
  (setf *debug-io* *standard-output*)
  (setf *trace-output* *standard-output*))


