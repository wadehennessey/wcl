;;; (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved.

(def-c-struct iobuf
  (int count)
  (char* pointer)
  (char* base)
  (int buffer-size)
  (short flag)
  (char  file))

(def-c-struct dirdesc
  (int file-desc)
  (long loc)
  (long size)
  (long bsize)
  (long off)
  (char* data-buffer))

(def-c-struct dirent
  (int offset)
  (int fileno)
  (short reclen)
  (short namlen)
  ((array char 256) name))

(def-c-type-name file-ptr (iobuf *))
(def-c-type-name dirdesc-ptr (dirdesc *))
(def-c-type-name dirent-ptr  (dirent *))

(defforeign opendir ((dirname char*) => (dirp dirdesc-ptr)))
(defforeign closedir ((dirp dirdesc-ptr) => (r int)))
(defforeign readdir ((dirp dirdesc-ptr) => (dp dirent-ptr)))

(defforeign alloc_memory ((len long) (word-size long) (type long) => (s t)))
(defforeign alloc_words ((len long) (type long) => (s t)))
(defforeign alloc_shorts ((len long) (type long) => (s t)))
(defforeign alloc_bytes ((len long) (type long) => (s t)))
(defforeign alloc_bits ((len long) (type long) => (s t)))
(defforeign c_cons ((car t) (cdr t) => (c t)))

(defforeign lisp_break (=> (v void)))
(defforeign lisp_debug (=> (v void)))

(defforeign command_line_argument ((n int) => (arg t)))

(defforeign apply_function ((argc t) (f t) (l t) => (r t)))

(defforeign initialize_array ((a t) (element_type_tag int) 
			      (element_size int) (element t) =>
			      (value t)))

(defforeign print_double_float ((n double) (stream file-ptr)
				=> (len int)))
(defforeign double_float_to_string ((n double) (stream char*) => (len int)))
(defforeign ilogb ((f double) => (exponent int)))

(defforeign add ((x t) (y t) => (sum t)))
(defforeign multiply ((x t) (y t) => (sum t)))
(defforeign subtract ((x t) (y t) => (sum t)))
(defforeign divide ((x t) (y t) => (sum t)))

(defforeign integer_add ((x t) (y t) => (sum t)))
(defforeign integer_subtract ((x t) (y t) => (sum t)))
(defforeign integer_multiply ((x t) (y t) => (sum t)))

(defforeign c_eql ((x t) (y t) => (flag t)))

(defforeign num_equal_p ((x t) (y t) => (flag t)))
(defforeign greaterp ((x t) (y t) => (flag t)))
(defforeign geq_p ((x t) (y t) => (flag t)))
(defforeign lessp ((x t) (y t) => (flag t)))
(defforeign leq_p ((x t) (y t) => (flag t)))

(defforeign make_symbol ((name t) (hash-code t) => (sym t)))
(defforeign make_static_symbol ((name t) (hash-code t) => (sym t)))

(defforeign dlopen ((file-name char*) (flags int) => (handle t)))
;;; HEY! dlsym really returns a uint32. Raw  ptr is returned here.
(defforeign dlsym ((handle t) (name char*) => (pointer t)))
(defforeign dlclose ((handle t) => (result int)))

(defforeign fopen ((fname char*) (type char*) => (file file-ptr)))
(defforeign fdopen ((file-desc int) (type char*) => (file file-ptr)))

(defforeign fclose ((stream file-ptr) => (status int)))
(defforeign fflush ((stream file-ptr) => (status int)))
(defforeign fseek ((stream file-ptr) (offset int) (flag int)
		   => (status int)))
(defforeign ftell ((stream file-ptr) => (length int)))

(defforeign unlink ((path char*) => (status int)))

(defforeign fgetc ((stream file-ptr) => (c int)))
(defforeign ungetc ((c char) (stream file-ptr) => (c int))) 
(defforeign fputc ((c char) (stream file-ptr) => (status int)))
(defforeign fputs ((c char*) (stream file-ptr) => (v void)))

;; HEY! Remove this when we can do a foreign aref on an array of structs 
(defforeign get_file_ptr ((index int) => (stream file-ptr)))

;; Only allow 0 as the eptr
(defforeign strtol ((digits (const char*)) (eptr (array char*)) (base int)
		    => (n long)))
(defforeign strtod ((digits (const char*)) (eptr (array char*))
		    => (f double)))
(defforeign strcmp ((s1 char*) (s2 char*) => (r int)))

(defforeign exit ((status int) => (v void)))
(defforeign system ((command (const char*)) => (status int)))
(defforeign popen ((command char*) (type char*) => (file file-ptr)))
(defforeign pclose ((file file-ptr) => (status int)))

(defforeign string_column ((string char*) (current int) => (new int)))
(defforeign fixnum_to_bignum ((x t) => (b t)))
(defforeign bignum_div ((x t) (y t) => (q t)))
(defforeign bignum_rem ((x t) (y t) => (r t)))
(defforeign bignum_logand ((x t) (y t) => (r t)))
(defforeign bignum_to_double ((x t) => (r double)))
(defforeign bignum_length ((x t) => (l t)))

(defforeign int_length ((x long) => (len int)))
(defforeign double_truncate ((x double) (y double) => (result int)))
(defforeign (double_floor "floor") ((x double) => (result double)))
(defforeign ceil ((x double) => (result double)))
(defforeign float_significand ((x double) => (result double)))
(defforeign float_exponent ((x double) => (result int)))
(defforeign ldexp ((significand double) (exponent int) => (result double)))
(defforeign fmod ((x double) (y double) => (result double)))
(defforeign rint ((x double) => (result double)))
(defforeign remainder ((x double) (y double) => (remainder double)))
(defforeign pow ((x double) (y double) => (result double)))

(defforeign (double_log "log") ((x double) => (result double)))
(defforeign (double_log10 "log10") ((x double) => (result double)))
(defforeign (double_sqrt "sqrt") ((x double) => (result double)))
(defforeign (double_exp "exp") ((x double) => (result double)))

(defforeign (double_sin "sin") ((x double) => (r double)))
(defforeign (double_cos "cos") ((x double) => (r double)))
(defforeign (double_tan "tan") ((x double) => (r double)))
(defforeign (double_asin "asin") ((x double) => (r double)))
(defforeign (double_acos "acos") ((x double) => (r double)))
(defforeign (double_atan2 "atan2") ((y double) (x double) => (r double)))

(defforeign heap_start (=> (result int)))
(defforeign heap_frontier (=> (result int)))
(defforeign heap_frontier_limit (=> (result int)))

(defforeign heap_page_size (=> (result int)))
(defforeign total_heap_pages (=> (result int)))
(defforeign free_heap_pages (=> (result int)))
(defforeign total_static_pages (=> (result int)))
(defforeign free_static_bytes (=> (result int)))

(defforeign unix_time_of_day (=> (result int)))
(defforeign unix_timezone (=> (result int)))
(defforeign unix_daylight_savings_time (=> (result int)))
(defforeign internal_real_time (=> (result int)))
(defforeign internal_system_run_time (=> (result int)))
(defforeign internal_user_run_time (=> (result int)))

(defforeign (c_sleep "sleep") ((seconds int) => (diff int)))

(defforeign c_aref ((array t) (indices t) => (result t)))
(defforeign c_set_aref ((value t) (array t) (indices t) =>
			(result t)))
(defforeign vref ((vector t) (index t) => (result t)))
(defforeign set_vref ((value t) (vector t) (index t) =>
		      (result t)))

;; CLX needs this
(defforeign connect_to_server ((host char*) (display int) => (value int)))

(defforeign full_gc (=> (v void)))
(defforeign gc_call_count (=> (c int)))
(defforeign set_gc_messages ((n int) => (value int)))
(defforeign intern_static_symbols (=> (v void)))
(defforeign switch_to_static_space (=> (v void)))
(defforeign switch_to_dynamic_space (=> (v void)))
(defforeign object_size ((ptr t) => (size-in-bytes int)))

(defforeign file_write_date ((filename char*) => (date int)))
(defforeign probe_file ((filename char*) => (status t)))
(defforeign file_length ((file file-ptr) => (date int)))
(defforeign file_listen ((file file-ptr) => (flag int)))
(defforeign write_byte ((data int) (file file-ptr) (element-type int)
			=> (status int)))
(defforeign read_byte
    ((file file-ptr) (element-type int) (eof-error-p t) (eof-value t)
     => (value t)))

(defforeign read_vector ((file file-ptr) (element-size int) (vector t)
			 (start int) (end int) (eof-error-p t)
			 (eof-value t) => (count t)))

(defforeign write_vector ((file file-ptr) (element-size int) (vector t)
			  (start int) (end int) => (count t)))


(defforeign chdir ((path char*) => (v int)))
(defforeign getpid (=> (pid int)))
(defforeign getenv ((name (const char*)) => (value char*)))
(defforeign putenv ((string char*) => (value int)))

(defforeign closure_oe((procedure t) => (oe t)))

(defforeign make_eval_closure((name t)
			      (formal-args t)
			      (body t)
			      (venv t)
			      (fenv t)
			      (tenv t)
			      (benv t) => (closure t)))

(defforeign gethostname ((name char*) (namelen int) => (status int)))
(defforeign getosversion ((name char*) (namelen int) => (status int)))
(defforeign getcwd ((path char*) (size int) => (value char*)))
(defforeign (c_random "random") (=> (r long)))
(defforeign initstate ((seed uint32) (data char*) (len unsigned-long)
                       => (r char*)))
(defforeign setstate ((data char*) => (r char*)))
(defforeign usleep ((useconds int) => (r void)))
(defforeign host_bits_per_word (=> (r int)))




