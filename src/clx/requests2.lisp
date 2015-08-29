;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package "XLIB")

(defun set-input-focus (display focus revert-to &optional time)
  (declare (type display display)
	   (type (or (member :none :pointer-root) window) focus)
	   (type (member :none :pointer-root :parent) revert-to)
	   (type timestamp time))
  (with-buffer-request (display *x-setinputfocus*)
    ((data (member :none :pointer-root :parent)) revert-to)
    ((or window (member :none :pointer-root)) focus)
    ((or null card32) time)))

(defun input-focus (display)
  (declare (type display display))
  (declare (values focus revert-to))
  (with-buffer-request-and-reply (display *x-getinputfocus* 16 :sizes (8 32))
       ()
    (values
      (or-get 8 (member :none :pointer-root) window)
      (member8-get 1 :none :pointer-root :parent))))

(defun query-keymap (display &optional bit-vector)
  (declare (type display display)
	   (type (or null (bit-vector 256)) bit-vector))
  (declare (values (bit-vector 256)))
  (with-buffer-request-and-reply (display *x-querykeymap* 40 :sizes 8)
       ()
    (values
      (bit-vector256-get 8 8 bit-vector))))

(defun create-pixmap (&key
		      pixmap
		      (width (required-arg width))
		      (height (required-arg height))
		      (depth (required-arg depth))
		      (drawable (required-arg drawable)))
  (declare (type (or null pixmap) pixmap)
	   (type card8 depth) ;; required
	   (type card16 width height) ;; required
	   (type drawable drawable)) ;; required
  (declare (values pixmap))
  (let* ((display (drawable-display drawable))
	 (pixmap (or pixmap (make-pixmap :display display)))
	 (pid (allocate-resource-id display pixmap 'pixmap)))
    (setf (pixmap-id pixmap) pid)
    (with-buffer-request (display *x-createpixmap*)
      (data depth)
      (resource-id pid)
      (drawable drawable)
      (card16 width height))
    pixmap))

(defun free-pixmap (pixmap)
  (declare (type pixmap pixmap))
  (let ((display (pixmap-display pixmap)))
    (with-buffer-request (display *x-freepixmap*)
      (pixmap pixmap))
    (deallocate-resource-id display (pixmap-id pixmap) 'pixmap)))

(defun clear-area (window &key (x 0) (y 0) width height exposures-p)
  ;; Passing in a zero width or height is a no-op.
  ;; A null width or height translates into a zero value in the protocol request.
  (declare (type window window)
	   (type int16 x y)
	   (type (or null card16) width height)
	   (type boolean exposures-p))
  (unless (or (eql width 0) (eql height 0))
    (with-buffer-request ((window-display window) *x-cleartobackground*)
      ((data boolean) exposures-p)
      (window window)
      (int16 x y)
      (card16 (or width 0) (or height 0)))))

(defun copy-area (src gcontext src-x src-y width height dst dst-x dst-y)
  (declare (type drawable src dst)
	   (type gcontext gcontext)
	   (type int16 src-x src-y dst-x dst-y)
	   (type card16 width height))
  (with-buffer-request ((drawable-display src) *x-copyarea* :gc-force gcontext)
    (drawable src dst)
    (gcontext gcontext)
    (int16 src-x src-y dst-x dst-y)
    (card16 width height)))

(defun copy-plane (src gcontext plane src-x src-y width height dst dst-x dst-y)
  (declare (type drawable src dst)
	   (type gcontext gcontext)
	   (type pixel plane)
	   (type int16 src-x src-y dst-x dst-y)
	   (type card16 width height))
  (with-buffer-request ((drawable-display src) *x-copyplane* :gc-force gcontext)
    (drawable src dst)
    (gcontext gcontext)
    (int16 src-x src-y dst-x dst-y)
    (card16 width height)
    (card32 plane)))

(defun create-colormap (visual-info window &optional alloc-p)
  (declare (type (or visual-info resource-id) visual-info)
	   (type window window)
	   (type boolean alloc-p))
  (declare (values colormap))
  (let ((display (window-display window)))
    (when (typep visual-info 'resource-id)
      (setf visual-info (visual-info display visual-info)))
    (let* ((colormap (make-colormap :display display :visual-info visual-info))
	   (id (allocate-resource-id display colormap 'colormap)))
      (setf (colormap-id colormap) id)
      (with-buffer-request (display *x-createcolormap*)
	((data boolean) alloc-p)
	(card29 id)
	(window window)
	(card29 (visual-info-id visual-info)))
      colormap)))

(defun free-colormap (colormap)
  (declare (type colormap colormap))
  (let ((display (colormap-display colormap)))
    (with-buffer-request (display *x-freecolormap*)
      (colormap colormap))
    (deallocate-resource-id display (colormap-id colormap) 'colormap)))

(defun copy-colormap-and-free (colormap)
  (declare (type colormap colormap))
  (declare (values colormap))
  (let* ((display (colormap-display colormap))
	 (new-colormap (make-colormap :display display
				      :visual-info (colormap-visual-info colormap)))
	 (id (allocate-resource-id display new-colormap 'colormap)))
    (setf (colormap-id new-colormap) id)
    (with-buffer-request (display *x-copycolormapandfree*)
      (resource-id id)
      (colormap colormap))
    new-colormap))

(defun install-colormap (colormap)
  (declare (type colormap colormap))
  (with-buffer-request ((colormap-display colormap) *x-installcolormap*)
    (colormap colormap)))

(defun uninstall-colormap (colormap)
  (declare (type colormap colormap))
  (with-buffer-request ((colormap-display colormap) *x-uninstallcolormap*)
    (colormap colormap)))

(defun installed-colormaps (window &key (result-type 'list))
  (declare (type window window)
	   (type t result-type)) ;; CL type
  (declare (values (sequence colormap)))
  (let ((display (window-display window)))
    (flet ((get-colormap (id)
	     (lookup-colormap display id)))
      (with-buffer-request-and-reply (display *x-listinstalledcolormaps* nil :sizes 16)
	   ((window window))
	(values
	  (sequence-get :result-type result-type :length (card16-get 8)
			:transform #'get-colormap :index *replysize*))))))

(defun alloc-color (colormap color)
  (declare (type colormap colormap)
	   (type (or stringable color) color))
  (declare (values pixel screen-color exact-color))
  (let ((display (colormap-display colormap)))
    (etypecase color
      (color
	(with-buffer-request-and-reply (display *x-alloccolor* 20 :sizes (16 32))
	     ((colormap colormap)
	      (rgb-val (color-red color)
		       (color-green color)
		       (color-blue color))
	      (pad16 nil))
	  (values
	    (card32-get 16)
	    (make-color :red (rgb-val-get 8)
			:green (rgb-val-get 10)
			:blue (rgb-val-get 12))
	    color)))
      (stringable
	(let* ((string (string color))
	       (length (length string)))
	  (with-buffer-request-and-reply (display *x-allocnamedcolor* 24 :sizes (16 32))
	       ((colormap colormap)
		(card16 length)
		(pad16 nil)
		(string string))
	    (values
	      (card32-get 8)
	      (make-color :red (rgb-val-get 18)
			  :green (rgb-val-get 20)
			  :blue (rgb-val-get 22))
	      (make-color :red (rgb-val-get 12)
			  :green (rgb-val-get 14)
			  :blue (rgb-val-get 16)))))))))

(defun alloc-color-cells (colormap colors &key (planes 0) contiguous-p (result-type 'list))
  (declare (type colormap colormap)
	   (type card16 colors planes)
	   (type boolean contiguous-p)
	   (type t result-type)) ;; CL type
  (declare (values (sequence pixel) (sequence mask)))
  (let ((display (colormap-display colormap)))
    (with-buffer-request-and-reply (display *x-alloccolorcells* nil :sizes 16)
	 (((data boolean) contiguous-p)
	  (colormap colormap)
	  (card16 colors planes))
      (let ((pixel-length (card16-get 8))
	    (mask-length (card16-get 10)))
	(values
	  (sequence-get :result-type result-type :length pixel-length :index *replysize*)
	  (sequence-get :result-type result-type :length mask-length
			:index (index+ *replysize* (index* pixel-length 4))))))))

(defun alloc-color-planes (colormap colors
			   &key (reds 0) (greens 0) (blues 0)
			   contiguous-p (result-type 'list))
  (declare (type colormap colormap)
	   (type card16 colors reds greens blues)
	   (type boolean contiguous-p)
	   (type t result-type)) ;; CL type
  (declare (values (sequence pixel) red-mask green-mask blue-mask))
  (let ((display (colormap-display colormap)))
    (with-buffer-request-and-reply (display *x-alloccolorplanes* nil :sizes (16 32))
	 (((data boolean) contiguous-p)
	  (colormap colormap)
	  (card16 colors reds greens blues))
      (let ((red-mask (card32-get 12))
	    (green-mask (card32-get 16))
	    (blue-mask (card32-get 20)))
	(values
	  (sequence-get :result-type result-type :length (card16-get 8) :index *replysize*)
	  red-mask green-mask blue-mask)))))

(defun free-colors (colormap pixels &optional (plane-mask 0))
  (declare (type colormap colormap)
	   (type sequence pixels) ;; Sequence of integers
	   (type pixel plane-mask))
  (with-buffer-request ((colormap-display colormap) *x-freecolors*)
    (colormap colormap)
    (card32 plane-mask)
    (sequence pixels)))

(defun store-color (colormap pixel spec &key (red-p t) (green-p t) (blue-p t))
  (declare (type colormap colormap)
	   (type pixel pixel)
	   (type (or stringable color) spec)
	   (type boolean red-p green-p blue-p))
  (let ((display (colormap-display colormap))
	(flags 0))
    (declare (type display display)
	     (type card8 flags))
    (when red-p (setq flags 1))
    (when green-p (incf flags 2))
    (when blue-p (incf flags 4))
    (etypecase spec
      (color
	(with-buffer-request (display *x-storecolors*)
	  (colormap colormap)
	  (card32 pixel)
	  (rgb-val (color-red spec)
		   (color-green spec)
		   (color-blue spec))
	  (card8 flags)
	  (pad8 nil)))
      (stringable
	(let* ((string (string spec))
	       (length (length string)))
	  (with-buffer-request (display *x-storenamedcolor*)
	    ((data card8) flags)
	    (colormap colormap)
	    (card32 pixel)
	    (card16 length)
	    (pad16 nil)
	    (string string)))))))

(defun store-colors (colormap specs &key (red-p t) (green-p t) (blue-p t))
  ;; If stringables are specified for colors, it is unspecified whether all
  ;; stringables are first resolved and then a single StoreColors protocol request is
  ;; issued, or whether multiple StoreColors protocol requests are issued.
  (declare (type colormap colormap)
	   (type sequence specs)
	   (type boolean red-p green-p blue-p))
  (etypecase specs
    (list
      (do ((spec specs (cddr spec)))
	  ((endp spec))
	(store-color colormap (car spec) (cadr spec) :red-p red-p :green-p green-p :blue-p blue-p)))
    (vector
      (do ((i 0 (+ i 2))
	   (len (length specs)))
	  ((>= i len))
	(store-color colormap (aref specs i) (aref specs (1+ i)) :red-p red-p :green-p green-p :blue-p blue-p)))))

(defun query-colors (colormap pixels &key (result-type 'list))
  (declare (type colormap colormap)
	   (type sequence pixels) ;; sequence of integer
	   (type t result-type))   ;; a type specifier
  (declare (values (sequence color)))
  (let ((display (colormap-display colormap)))
    (with-buffer-request-and-reply (display *x-querycolors* nil :sizes (8 16))
	 ((colormap colormap)
	  (sequence pixels))
      (let ((sequence (make-sequence result-type (card16-get 8))))
	(advance-buffer-offset *replysize*)
	(dotimes (i (length sequence) sequence)
	  (setf (elt sequence i)
		(make-color :red (rgb-val-get 0)
			    :green (rgb-val-get 2)
			    :blue (rgb-val-get 4)))
	  (advance-buffer-offset 8))))))

(defun lookup-color (colormap name)
  (declare (type colormap colormap)
	   (type stringable name))
  (declare (values screen-color true-color))
  (let* ((display (colormap-display colormap))
	 (string (string name))
	 (length (length string)))
    (with-buffer-request-and-reply (display *x-lookupcolor* 20 :sizes 16)
	 ((colormap colormap)
	  (card16 length)
	  (pad16 nil)
	  (string string))
      (values
	(make-color :red (rgb-val-get 14)
		    :green (rgb-val-get 16)
		    :blue (rgb-val-get 18))
	(make-color :red (rgb-val-get 8)
		    :green (rgb-val-get 10)
		    :blue (rgb-val-get 12))))))

(defun create-cursor (&key
		      (source (required-arg source))
		      mask
		      (x (required-arg x))
		      (y (required-arg y))
		      (foreground (required-arg foreground))
		      (background (required-arg background)))
  (declare (type pixmap source) ;; required
	   (type (or null pixmap) mask)
	   (type card16 x y) ;; required
	   (type (or null color) foreground background)) ;; required
  (declare (values cursor))
  (let* ((display (pixmap-display source))
	 (cursor (make-cursor :display display))
	 (cid (allocate-resource-id display cursor 'cursor)))
    (setf (cursor-id cursor) cid)
    (with-buffer-request (display *x-createcursor*)
      (resource-id cid)
      (pixmap source)
      ((or null pixmap) mask)
      (rgb-val (color-red foreground)
	       (color-green foreground)
	       (color-blue foreground))
      (rgb-val (color-red background)
	       (color-green background)
	       (color-blue background))
      (card16 x y))
    cursor))

(defun create-glyph-cursor (&key
			    (source-font (required-arg source-font))
			    (source-char (required-arg source-char))
			    mask-font
			    mask-char
			    (foreground (required-arg foreground))
			    (background (required-arg background)))
  (declare (type font source-font) ;; Required
	   (type card16 source-char) ;; Required
	   (type (or null font) mask-font)
	   (type (or null card16) mask-char)
	   (type color foreground background)) ;; required
  (declare (values cursor))
  (let* ((display (font-display source-font))
	 (cursor (make-cursor :display display))
	 (cid (allocate-resource-id display cursor 'cursor))
	 (source-font-id (font-id source-font))
	 (mask-font-id (if mask-font (font-id mask-font) 0)))
    (setf (cursor-id cursor) cid)
    (unless mask-char (setq mask-char 0))
    (with-buffer-request (display *x-createglyphcursor*)
      (resource-id cid source-font-id mask-font-id)
      (card16 source-char)
      (card16 mask-char)
      (rgb-val (color-red foreground)
	       (color-green foreground)
	       (color-blue foreground))
      (rgb-val (color-red background)
	       (color-green background)
	       (color-blue background)))
    cursor))

(defun free-cursor (cursor)
  (declare (type cursor cursor))
  (let ((display (cursor-display cursor)))
    (with-buffer-request (display *x-freecursor*)
      (cursor cursor))
    (deallocate-resource-id display (cursor-id cursor) 'cursor)))

(defun recolor-cursor (cursor foreground background)
  (declare (type cursor cursor)
	   (type color foreground background))
  (with-buffer-request ((cursor-display cursor) *x-recolorcursor*)
    (cursor cursor)
    (rgb-val (color-red foreground)
	     (color-green foreground)
	     (color-blue foreground))
    (rgb-val (color-red background)
	     (color-green background)
	     (color-blue background))
    ))

(defun query-best-cursor (width height drawable)
  (declare (type card16 width height)
	   (type (or drawable display) drawable))	
  (declare (values width height))
  ;; Drawable can be a display for compatibility.
  (multiple-value-bind (display drawable)
      (if (type? drawable 'drawable)
	  (values (drawable-display drawable) drawable)
	(values drawable (screen-root (display-default-screen drawable))))
    (with-buffer-request-and-reply (display *x-querybestsize* 12 :sizes 16)
	 ((data 0)
	  (window drawable)
	  (card16 width height))
      (values
	(card16-get 8)
	(card16-get 10)))))

(defun query-best-tile (width height drawable)
  (declare (type card16 width height)
	   (type drawable drawable))
  (declare (values width height))
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display *x-querybestsize* 12 :sizes 16)
	 ((data 1)
	  (drawable drawable)
	  (card16 width height))
      (values
	(card16-get 8)
	(card16-get 10)))))

(defun query-best-stipple (width height drawable)
  (declare (type card16 width height)
	   (type drawable drawable))
  (declare (values width height))
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display *x-querybestsize* 12 :sizes 16)
	 ((data 2)
	  (drawable drawable)
	  (card16 width height))
      (values
	(card16-get 8)
	(card16-get 10)))))

(defun query-extension (display name)
  (declare (type display display)
	   (type stringable name))
  (declare (values major-opcode first-event first-error))
  (let ((string (string name)))
    (with-buffer-request-and-reply (display *x-queryextension* 12 :sizes 8)
	 ((card16 (length string))
	  (pad16 nil)
	  (string string))
      (and (boolean-get 8)    ;; If present
	   (values
	     (card8-get 9)
	     (card8-get 10)
	     (card8-get 11))))))

(defun list-extensions (display &key (result-type 'list))
  (declare (type display display)
	   (type t result-type)) ;; CL type
  (declare (values (sequence string)))
  (with-buffer-request-and-reply (display *x-listextensions* size :sizes 8)
       ()
    (values
      (read-sequence-string
	buffer-bbuf (index- size *replysize*) (card8-get 1) result-type *replysize*))))

(defun change-keyboard-control (display &key key-click-percent
				bell-percent bell-pitch bell-duration
				led led-mode key auto-repeat-mode)
  (declare (type display display)
	   (type (or null (member :default) int16) key-click-percent
						   bell-percent bell-pitch bell-duration)
	   (type (or null card8) led key)
	   (type (or null (member :on :off)) led-mode)
	   (type (or null (member :on :off :default)) auto-repeat-mode))
  (when (eq key-click-percent :default) (setq key-click-percent -1))
  (when (eq bell-percent :default) (setq bell-percent -1))
  (when (eq bell-pitch :default) (setq bell-pitch -1))
  (when (eq bell-duration :default) (setq bell-duration -1))
  (with-buffer-request (display *x-changekeyboardcontrol* :sizes (32))
    (mask
      (integer key-click-percent bell-percent bell-pitch bell-duration)
      (card32 led)
      ((member :off :on) led-mode)
      (card32 key)
      ((member :off :on :default) auto-repeat-mode))))

(defun keyboard-control (display)
  (declare (type display display))
  (declare (values key-click-percent bell-percent bell-pitch bell-duration
		  led-mask global-auto-repeat auto-repeats))
  (with-buffer-request-and-reply (display *x-getkeyboardcontrol* 32 :sizes (8 16 32))
       ()
    (values
      (card8-get 12)
      (card8-get 13)
      (card16-get 14)
      (card16-get 16)
      (card32-get 8)
      (member8-get 1 :off :on)
      (bit-vector256-get 32))))

;;  The base volume should
;; be considered to be the "desired" volume in the normal case; that is, a
;; typical application should call XBell with 0 as the percent.  Rather
;; than using a simple sum, the percent argument is instead used as the
;; percentage of the remaining range to alter the base volume by.  That is,
;; the actual volume is:
;;	 if percent>=0:    base - [(base * percent) / 100] + percent
;;	 if percent<0:     base + [(base * percent) / 100]

(defun bell (display &optional (percent-from-normal 0))
  ;; It is assumed that an eventual audio extension to X will provide more complete control.
  (declare (type display display)
	   (type int8 percent-from-normal))
  (with-buffer-request (display *x-bell*)
    (data (int8->card8 percent-from-normal))))

(defun pointer-mapping (display &key (result-type 'list))
  (declare (type display display)
	   (type t result-type)) ;; CL type
  (declare (values sequence)) ;; Sequence of card
  (with-buffer-request-and-reply (display *x-getpointermapping* nil :sizes 8)
       ()
    (values
      (sequence-get :length (card8-get 1) :result-type result-type :format card8
		    :index *replysize*))))

(defun set-pointer-mapping (display map)
  ;; Can signal device-busy.
  (declare (type display display)
	   (type sequence map)) ;; Sequence of card8
  (when (with-buffer-request-and-reply (display *x-setpointermapping* 2 :sizes 8)
	     ((data (length map))
	      ((sequence :format card8) map))
	  (values
	    (boolean-get 1)))
    (error 'device-busy :display display))
  map)

(defsetf pointer-mapping set-pointer-mapping)

(defun change-pointer-control (display &key acceleration threshold)
  ;; Acceleration is rationalized if necessary.
  (declare (type display display)
	   (type (or null (member :default) number) acceleration)
	   (type (or null (member :default) integer) threshold)
	   (inline rationalize16))
  (flet ((rationalize16 (number)
	   ;; Rationalize NUMBER into the ratio of two signed 16 bit numbers
	   (declare (type number number)
		    (inline rationalize16))
	   (declare (values numerator denominator))
	   (do* ((rational (rationalize number))
		 (numerator (numerator rational) (ash numerator -1))
		 (denominator (denominator rational) (ash denominator -1)))
		((or (= numerator 1)
		     (and (< (abs numerator) #x8000)
			  (< denominator #x8000)))
		 (values
		   numerator (min denominator #x7fff))))))

    (let ((acceleration-p 1)
	  (threshold-p 1)
	  (numerator 0)
	  (denominator 1))
      (declare (type card8 acceleration-p threshold-p)
	       (type int16 numerator denominator))
      (cond ((eq acceleration :default) (setq numerator -1))
	    (acceleration (multiple-value-setq (numerator denominator)
			    (rationalize16 acceleration)))
	    (t (setq acceleration-p 0)))
      (cond ((eq threshold :default) (setq threshold -1))
	    ((null threshold) (setq threshold -1
				    threshold-p 0)))
      (with-buffer-request (display *x-changepointercontrol*)
	(int16 numerator denominator threshold)
	(card8 acceleration-p threshold-p)))))

(defun pointer-control (display)
  (declare (type display display))
  (declare (values acceleration threshold))
  (with-buffer-request-and-reply (display *x-getpointercontrol* 16 :sizes 16)
       ()
    (values
      (/ (card16-get 8) (card16-get 10)		; Should we float this?
	 (card16-get 12)))))

(defun set-screen-saver (display timeout interval blanking exposures)
  ;; Timeout and interval are in seconds, will be rounded to minutes.
  (declare (type display display)
	   (type (or (member :default) int16) timeout interval)
	   (type (member :on :off :default :yes :no) blanking exposures))
  (case blanking (:yes (setq blanking :on)) (:no (setq blanking :off)))
  (case exposures (:yes (setq exposures :on)) (:no (setq exposures :off)))
  (when (eq timeout :default) (setq timeout -1))
  (when (eq interval :default) (setq interval -1))
  (with-buffer-request (display *x-setscreensaver*)
    (int16 timeout interval)
    ((member8 :on :off :default) blanking exposures)))

(defun screen-saver (display)
  ;; Returns timeout and interval in seconds.
  (declare (type display display))
  (declare (values timeout interval blanking exposures))
  (with-buffer-request-and-reply (display *x-getscreensaver* 14 :sizes (8 16))
       ()
    (values
      (card16-get 8)
      (card16-get 10)
      (member8-get 12 :on :off :default)
      (member8-get 13 :on :off :default))))

(defun activate-screen-saver (display)
  (declare (type display display))
  (with-buffer-request (display *x-forcescreensaver*)
    (data 1)))

(defun reset-screen-saver (display)
  (declare (type display display))
  (with-buffer-request (display *x-forcescreensaver*)
    (data 0)))

(defun add-access-host (display host &optional (family :internet))
  ;; A string must be acceptable as a host, but otherwise the possible types for
  ;; host are not constrained, and will likely be very system dependent.
  ;; This implementation uses a list whose car is the family keyword
  ;; (:internet :DECnet :Chaos) and cdr is a list of network address bytes.
  (declare (type display display)
	   (type (or stringable list) host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (change-access-host display host family nil))

(defun remove-access-host (display host &optional (family :internet))
  ;; A string must be acceptable as a host, but otherwise the possible types for
  ;; host are not constrained, and will likely be very system dependent.
  ;; This implementation uses a list whose car is the family keyword
  ;; (:internet :DECnet :Chaos) and cdr is a list of network address bytes.
  (declare (type display display)
	   (type (or stringable list) host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (change-access-host display host family t))

(defun change-access-host (display host family remove-p)
  (declare (type display display)
	   (type (or stringable list) host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (unless (consp host)
    (setq host (host-address host family)))
  (let ((family (car host))
	(address (cdr host)))
    (with-buffer-request (display *x-changehosts*)
      ((data boolean) remove-p)
      (card8 (encode-type (or null (member :internet :decnet :chaos) card32) family))
      (card16 (length address))
      ((sequence :format card8) address))))

(defun access-hosts (display &optional (result-type 'list))
  ;; The type of host objects returned is not constrained, except that the hosts must
  ;; be acceptable to add-access-host and remove-access-host.
  ;; This implementation uses a list whose car is the family keyword
  ;; (:internet :DECnet :Chaos) and cdr is a list of network address bytes.
  (declare (type display display)
	   (type t result-type)) ;; CL type
  (declare (values (sequence host) enabled-p))
  (with-buffer-request-and-reply (display *x-listhosts* nil :sizes (8 16))
       ()
    (let* ((enabled-p (boolean-get 1))
	   (nhosts (card16-get 8))
	   (sequence (make-sequence result-type nhosts)))
      (advance-buffer-offset *replysize*)
      (dotimes (i nhosts)
	(let ((family (card8-get 0))
	      (len (card16-get 2)))
	  (setf (elt sequence i)
		(cons (if (< family 3)
			  (svref '#(:internet :decnet :chaos) family)
			family)
		      (sequence-get :length len :format card8 :result-type 'list
				    :index (+ buffer-boffset 4))))
	  (advance-buffer-offset (+ 4 (* 4 (ceiling len 4))))))
      (values
	sequence
	enabled-p))))

(defun access-control (display)
  (declare (type display display))
  (declare (values boolean)) ;; True when access-control is ENABLED
  (with-buffer-request-and-reply (display *x-listhosts* 2 :sizes 8)
       ()
    (boolean-get 1)))
  
(defun set-access-control (display enabled-p)
  (declare (type display display)
	   (type boolean enabled-p))
  (with-buffer-request (display *x-changeaccesscontrol*)
    ((data boolean) enabled-p))
  enabled-p)

(defsetf access-control set-access-control)

(defun close-down-mode (display)
  ;; setf'able
  ;; Cached locally in display object.
  (declare (type display display))
  (declare (values (member :destroy :retain-permanent :retain-temporary nil)))
  (display-close-down-mode display))

(defun set-close-down-mode (display mode)
  ;; Cached locally in display object.
  (declare (type display display)
	   (type (member :destroy :retain-permanent :retain-temporary) mode))
  (setf (display-close-down-mode display) mode)
  (with-buffer-request (display *x-changeclosedownmode* :sizes (32))
    ((data (member :destroy :retain-permanent :retain-temporary)) mode))
  mode)

(defsetf close-down-mode set-close-down-mode)

(defun kill-client (display resource-id)
  (declare (type display display)
	   (type resource-id resource-id))
  (with-buffer-request (display *x-killclient*)
    (resource-id resource-id)))

(defun kill-temporary-clients (display)
  (declare (type display display))
  (with-buffer-request (display *x-killclient*)
    (resource-id 0)))

(defun no-operation (display)
  (declare (type display display))
  (with-buffer-request (display *x-nooperation*)))
