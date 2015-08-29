;;; Copyright (C) 1988 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Run WCL under Emacs. This is a really weak hack that needs more work.
;;;
;;; Based on GDB mode:
;;;   Author: W. Schelter, University of Texas
;;;           wfs@rascal.ics.utexas.edu
;;;   Rewritten by rms.
;;;   Some ideas are due to  Masanobu. 
;;;
;;; To use this package:
;;; set wcl-root-directory and wcl-bin
;;; for your site, then use run-wcl to start a wcl.
;;; c-m-x will zap defuns to wcl.

(require 'shell)

;;; Set these for your site.
(defvar wcl-root-directory "/home/wade/w/current/")

(defvar wcl-bin "bin/wcl")

(defun wcl-path ()
  (concat wcl-root-directory wcl-bin))

(defun wcl-gdb-init-path ()
  (concat wcl-root-directory "emacs/gdb-init.gdb"))

(defvar wcl-command-name "gdb" "Pathname for executing gdb.")

(defvar current-wcl-buffer nil)

(defvar wcl-prompt-pattern "^(.*wcl[+]?) *"
  "A regexp to recognize the prompt for wcl or wcl+.") 

(defvar wcl-mode-map nil
  "Keymap for wcl-mode.")

(if wcl-mode-map
   nil
  (setq wcl-mode-map (copy-keymap shell-mode-map))
  (define-key wcl-mode-map "\C-l" 'wcl-refresh))

(define-key ctl-x-map " " 'wcl-break)
(define-key ctl-x-map "&" 'send-wcl-command)

(defun wcl-mode ()
  "Major mode for interacting with an inferior Wcl process.
Many commands are inherited from shell mode. 
Additionally we have:
C-x SPACE sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'wcl-mode)
  (setq mode-name "Inferior Wcl")
  (setq mode-line-process '(": %s"))
  (use-local-map wcl-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'wcl-last-frame)
  (setq wcl-last-frame nil)
  (make-local-variable 'wcl-last-frame-displayed-p)
  (setq wcl-last-frame-displayed-p t)
  (make-local-variable 'wcl-delete-prompt-marker)
  (setq wcl-delete-prompt-marker nil)
  (make-local-variable 'wcl-filter-accumulator)
  (setq wcl-filter-accumulator nil)
  (make-local-variable 'shell-prompt-pattern)
  (setq shell-prompt-pattern wcl-prompt-pattern)
  (run-hooks 'shell-mode-hook 'wcl-mode-hook))

(defun wcl (path)
  "Run wcl on program FILE in buffer *wcl-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for WCL.  If you wish to change this, use
the WCL commands `cd DIR' and `directory'."
  (interactive "FRun wcl on file: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*wcl-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (make-shell (concat "wcl-" file) wcl-command-name nil "-fullname"
		"-cd" default-directory file)
    (wcl-mode)
    (set-process-filter (get-buffer-process (current-buffer)) 'wcl-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'wcl-sentinel)
    (wcl-set-buffer)))

(defun wcl-set-buffer ()
  (cond ((eq major-mode 'wcl-mode)
	(setq current-wcl-buffer (current-buffer)))))

;; This function is responsible for inserting output from WCL
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that WCL prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun wcl-filter (proc string)
  (let ((inhibit-quit t))
    (if wcl-filter-accumulator
	(wcl-filter-accumulate-marker proc
				      (concat wcl-filter-accumulator string))
	(wcl-filter-scan-input proc string))))

(defun wcl-filter-accumulate-marker (proc string)
  (setq wcl-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq wcl-last-frame
			  (cons (substring string 2 first-colon)
				(string-to-int
				 (substring string (1+ first-colon)
					    second-colon)))))
		  (setq wcl-last-frame-displayed-p nil)
		  (wcl-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq wcl-filter-accumulator string)))
	(wcl-filter-insert proc "\032")
	(wcl-filter-scan-input proc (substring string 1)))
    (setq wcl-filter-accumulator string)))

(defun wcl-filter-scan-input (proc string)
  (if (equal string "")
      (setq wcl-filter-accumulator nil)
      (let ((start (string-match "\032" string)))
	(if start
	    (progn (wcl-filter-insert proc (substring string 0 start))
		   (wcl-filter-accumulate-marker proc
						 (substring string start)))
	    (wcl-filter-insert proc string)))))

(defun wcl-filter-insert (proc string)
  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc)))
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (wcl-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (wcl-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t))
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))

(defun wcl-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the wcl buffer.
	     (set-buffer obuf))))))


(defun wcl-refresh ()
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (wcl-display-frame))

(defun wcl-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from WCL.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (wcl-set-buffer)
  (and wcl-last-frame (not nodisplay)
       (or (not wcl-last-frame-displayed-p) (not noauto))
       (progn (wcl-display-line (car wcl-last-frame) (cdr wcl-last-frame))
	      (setq wcl-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun wcl-display-line (true-file line)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defun wcl-call (command)
  "Invoke wcl COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  (setq wcl-delete-prompt-marker (point-marker))
  (wcl-set-buffer)
  (send-string (get-buffer-process current-wcl-buffer)
	       (concat command "\n")))

(defun wcl-maybe-delete-prompt ()
  (if (and wcl-delete-prompt-marker
	   (> (point-max) (marker-position wcl-delete-prompt-marker)))
      (let (start)
	(goto-char wcl-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq wcl-delete-prompt-marker nil))))

(defun wcl-break ()
  "Set WCL breakpoint at this source line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(1+ (count-lines 1 (point))))))
    (send-string (get-buffer-process current-wcl-buffer)
		 (concat "break " file-name ":" line "\n"))))

(defun wcl-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (dot)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(dot)))
     (cond (found (forward-char 2)(setq result
			(buffer-substring found
				 (progn (re-search-forward "[^0-9a-f]")
					(forward-char -1)
					(dot)))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (dot)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (dot)))))))


(defvar wcl-commands nil
  "List of strings or functions used by send-wcl-command.
It is for customization by you.")

(defun send-wcl-command (arg)
  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the wcl buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list wcl-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of wcl-commands.  "
  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg wcl-commands)))
    (setq addr (wcl-read-address))
    (if (eq (current-buffer) current-wcl-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-wcl-buffer)
    (goto-char (dot-max))
    (insert-string comm)))

(defvar inferior-lisp-mode-map nil)

(if inferior-lisp-mode-map
    nil
  (setq inferior-lisp-mode-map (copy-alist shell-mode-map))
  (lisp-mode-commands inferior-lisp-mode-map)
  (define-key inferior-lisp-mode-map "\e\C-x" 'lisp-send-defun))

(defvar inferior-lisp-load-command "(load \"%s\")\n"
  "*Format-string for building a Lisp expression to load a file.
This format string should use %s to substitute a file name
and should result in a Lisp expression that will command the inferior Lisp
to load that file.  The default works acceptably on most Lisps.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\\n\"
produces cosmetically superior output for this application,
but it works only in Common Lisp.")

(defvar inferior-lisp-prompt "^.*>:? *$"
  "*Regexp to recognize prompts from the inferior Lisp.")

(defun run-wcl ()
  (interactive)
  (switch-to-buffer (make-shell "lisp"
				wcl-command-name nil
				"-quiet"
				(concat "-command=" (wcl-gdb-init-path))
				"-fullname"
				(wcl-path)))
  (wcl-mode)
  (set-process-filter (get-buffer-process (current-buffer)) 'wcl-filter)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'wcl-sentinel)
  (wcl-set-buffer))

(defun lisp-send-defun (display-flag)
  "Send the current defun to the Lisp process made by M-x run-lisp.
With argument, force redisplay and scrolling of the *lisp* buffer.
Variable `inferior-lisp-load-command' controls formatting of
the `load' form that is set to the Lisp process."
  (interactive "P")
  (or (get-process "lisp")
      (error "No current lisp process"))
  (save-excursion
    (end-of-defun)
    (let ((end (point))
	  (filename (format "/tmp/emlisp%d.lisp" (process-id (get-process "lisp")))))
      (beginning-of-defun)
      (write-region (point) end filename nil 'nomessage)
      (process-send-string "lisp" (format inferior-lisp-load-command filename)))
    (if display-flag
	(let* ((process (get-process "lisp"))
	       (buffer (process-buffer process))
	       (w (or (get-buffer-window buffer) (display-buffer buffer)))
	       (height (window-height w))
	       (end))
	  (save-excursion
	    (set-buffer buffer)
	    (setq end (point-max))
	    (while (progn
		     (accept-process-output process)
		     (goto-char (point-max))
		     (beginning-of-line)
		     (or (= (point-max) end)
			 (not (looking-at inferior-lisp-prompt)))))
	    (setq end (point-max))
	    (vertical-motion (- 4 height))
	    (set-window-start w (point)))
	  (set-window-point w end)))))
