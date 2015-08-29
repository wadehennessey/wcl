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

(defun create-window (&key
		      window
		      (parent (required-arg parent))
		      (x (required-arg x))
		      (y (required-arg y))
		      (width (required-arg width))
		      (height (required-arg height))
		      (depth 0) (border-width 0)
		      (class :copy) (visual :copy)
		      background border
		      bit-gravity gravity
		      backing-store backing-planes backing-pixel save-under
		      event-mask do-not-propagate-mask override-redirect
		      colormap cursor)
  ;; Display is obtained from parent.  Only non-nil attributes are passed on in
  ;; the request: the function makes no assumptions about what the actual protocol
  ;; defaults are.  Width and height are the inside size, excluding border.
  (declare (type (or null window) window)
	   (type window parent)		; required
	   (type int16 x y) ;required
	   (type card16 width height) ;required
	   (type card16 depth border-width)
	   (type (member :copy :input-output :input-only) class)
	   (type (or (member :copy) visual-info resource-id) visual)
	   (type (or null (member :none :parent-relative) pixel pixmap) background)
	   (type (or null (member :copy) pixel pixmap) border)
	   (type (or null bit-gravity) bit-gravity)
	   (type (or null win-gravity) gravity)
	   (type (or null (member :not-useful :when-mapped :always)) backing-store)
	   (type (or null pixel) backing-planes backing-pixel)
	   (type (or null event-mask) event-mask)
	   (type (or null device-event-mask) do-not-propagate-mask)
	   (type (or null (member :on :off)) save-under override-redirect)
	   (type (or null (member :copy) colormap) colormap)
	   (type (or null (member :none) cursor) cursor))
  (declare (values window))
  (let* ((display (window-display parent))
	 (window (or window (make-window :display display)))
	 (wid (allocate-resource-id display window 'window))
	 back-pixmap back-pixel
	 border-pixmap border-pixel)
    (declare (type display display)
	     (type window window)
	     (type resource-id wid)
	     (type (or null resource-id) back-pixmap border-pixmap)
	     (type (or null pixel) back-pixel border-pixel))
    (setf (window-id window) wid)
    (case background
      ((nil) nil)
      (:none (setq back-pixmap 0))
      (:parent-relative (setq back-pixmap 1))
      (otherwise
       (if (type? background 'pixmap)
	   (setq back-pixmap (pixmap-id background))
	 (if (integerp background)
	     (setq back-pixel background)
	   (x-type-error background
			 '(or null (member :none :parent-relative) integer pixmap))))))
    (case border
      ((nil) nil)
      (:copy (setq border-pixmap 0))
      (otherwise
       (if (type? border 'pixmap)
	   (setq border-pixmap (pixmap-id border))
	 (if (integerp border)
	     (setq border-pixel border)
	   (x-type-error border '(or null (member :copy) integer pixmap))))))
    (when event-mask
      (setq event-mask (encode-event-mask event-mask)))
    (when do-not-propagate-mask
      (setq do-not-propagate-mask (encode-device-event-mask do-not-propagate-mask)))

						;Make the request
    (with-buffer-request (display *x-createwindow*)
      (data depth)
      (resource-id wid)
      (window parent)
      (int16 x y)
      (card16 width height border-width)
      ((member16 :copy :input-output :input-only) class)
      (resource-id (cond ((eq visual :copy)
			  0)
			 ((typep visual 'resource-id)
			  visual)
			 (t
			  (visual-info-id visual))))
      (mask (card32 back-pixmap back-pixel border-pixmap border-pixel)
	    ((member-vector *bit-gravity-vector*) bit-gravity)
	    ((member-vector *win-gravity-vector*) gravity)
	    ((member :not-useful :when-mapped :always) backing-store)
	    (card32  backing-planes backing-pixel)
	    ((member :off :on) override-redirect save-under)
	    (card32 event-mask do-not-propagate-mask)
	    ((or (member :copy) colormap) colormap)
	    ((or (member :none) cursor) cursor)))
    window))

(defun destroy-window (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-destroywindow*)
    (window window)))

(defun destroy-subwindows (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-destroysubwindows*)
    (window window)))

(defun add-to-save-set (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-changesaveset*)
    (data 0)
    (window window)))

(defun remove-from-save-set (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-changesaveset*)
    (data 1)
    (window window)))

(defun reparent-window (window parent x y)
  (declare (type window window parent)
	   (type int16 x y))
  (with-buffer-request ((window-display window) *x-reparentwindow*)
    (window window parent)
    (int16 x y)))

(defun map-window (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-mapwindow*)
    (window window)))

(defun map-subwindows (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-mapsubwindows*)
    (window window)))

(defun unmap-window (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-unmapwindow*)
    (window window)))

(defun unmap-subwindows (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-unmapsubwindows*)
    (window window)))

(defun circulate-window-up (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-circulatewindow*)
    (data 0)
    (window window)))

(defun circulate-window-down (window)
  (declare (type window window))
  (with-buffer-request ((window-display window) *x-circulatewindow*)
    (data 1)
    (window window)))

(defun query-tree (window &key (result-type 'list))
  (declare (type window window)
	   (type t result-type)) ;;type specifier
  (declare (values (sequence window) parent root))
  (let ((display (window-display window)))
    (multiple-value-bind (root parent sequence)
	(with-buffer-request-and-reply (display *x-querytree* nil :sizes (8 16 32))
	     ((window window))
	  (values
	    (window-get 8)
	    (resource-id-get 12)
	    (sequence-get :length (card16-get 16) :result-type result-type
			  :index *replysize*)))
      ;; Parent is NIL for root window
      (setq parent (and (plusp parent) (lookup-window display parent)))
      (dotimes (i (length sequence))		; Convert ID's to window's
	(setf (elt sequence i) (lookup-window display (elt sequence i))))
      (values sequence parent root))))

;; Although atom-ids are not visible in the normal user interface, atom-ids might
;; appear in window properties and other user data, so conversion hooks are needed.

(defun intern-atom (display name)
  (declare (type display display)
	   (type xatom name))
  (declare (values resource-id))
  (let ((name (if (or (null name) (keywordp name))
		  name
		(kintern (string name)))))
    (declare (type symbol name))
    (or (atom-id name display)
	(let ((string (symbol-name name)))
	  (declare (type string string))
	  (multiple-value-bind (id)
	      (with-buffer-request-and-reply (display *x-internatom* 12 :sizes 32)
		   ((data 0)
		    (card16 (length string))
		    (pad16 nil)
		    (string string))
		(values
		  (resource-id-get 8)))
	    (declare (type resource-id id))
	    (setf (atom-id name display) id)
	    id)))))

(defun find-atom (display name)
  ;; Same as INTERN-ATOM, but with the ONLY-IF-EXISTS flag True
  (declare (type display display)
	   (type xatom name))
  (declare (values (or null resource-id)))
  (let ((name (if (or (null name) (keywordp name))
		  name
		(kintern (string name)))))
    (declare (type symbol name))
    (or (atom-id name display)
	(let ((string (symbol-name name)))
	  (declare (type string string))
	  (multiple-value-bind (id)
	      (with-buffer-request-and-reply (display *x-internatom* 12 :sizes 32)
		   ((data 1)
		    (card16 (length string))
		    (pad16 nil)
		    (string string))
		(values
		  (or-get 8 null resource-id)))
	    (declare (type (or null resource-id) id))
	    (when id 
	      (setf (atom-id name display) id))
	    id)))))

(defun atom-name (display atom-id)
  (declare (type display display)
	   (type resource-id atom-id))
  (declare (values keyword))
  (if (zerop atom-id)
      nil
  (or (id-atom atom-id display)
      (let ((keyword
	      (kintern
		  (with-buffer-request-and-reply
		       (display *x-getatomname* nil :sizes (16))
		     ((resource-id atom-id))
		  (values
		    (string-get (card16-get 8) *replysize*))))))
	(declare (type keyword keyword))
	(setf (atom-id keyword display) atom-id)
	  keyword))))

;;; For binary compatibility with older code
(defun lookup-xatom (display atom-id)
  (declare (type display display)
	   (type resource-id atom-id))
  (atom-name display atom-id))

(defun change-property (window property data type format
		       &key (mode :replace) (start 0) end transform)
  ; Start and end affect sub-sequence extracted from data.
  ; Transform is applied to each extracted element.
  (declare (type window window)
	   (type xatom property type)
	   (type (member 8 16 32) format)
	   (type sequence data)
	   (type (member :replace :prepend :append) mode)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type t transform))			;(or null (function (t) integer))
  (unless end (setq end (length data)))
  (let* ((display (window-display window))
	 (length (index- end start))
	 (property-id (intern-atom display property))
	 (type-id (intern-atom display type)))
    (declare (type display display)
	     (type array-index length)
	     (type resource-id property-id type-id))
    (with-buffer-request (display *x-changeproperty*)
      ((data (member :replace :prepend :append)) mode)
      (window window)
      (resource-id property-id type-id)
      (card8 format)
      (card32 length)
      (progn
	(ecase format
	  (8  (sequence-put 24 data :format card8
			    :start start :end end :transform transform))
	  (16 (sequence-put 24 data :format card16
			    :start start :end end :transform transform))
	  (32 (sequence-put 24 data :format card32
			    :start start :end end :transform transform)))))))

(defun delete-property (window property)
  (declare (type window window)
	   (type xatom property))
  (let* ((display (window-display window))
	 (property-id (intern-atom display property)))
    (declare (type display display)
	     (type resource-id property-id))
    (with-buffer-request (display *x-deleteproperty*)
      (window window)
      (resource-id property-id))))

(defun get-property (window property
		     &key type (start 0) end delete-p (result-type 'list) transform)
  ;; Transform is applied to each integer retrieved.
  (declare (type window window)
	   (type xatom property)
	   (type (or null xatom) type)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type boolean delete-p)
	   (type t result-type)			;a sequence type
	   (type t transform))			;(or null (function (integer) t))
  (declare (values data (or null type) format bytes-after))
  (let* ((display (window-display window))
	 (property-id (intern-atom display property))
	 (type-id (and type (intern-atom display type))))
    (declare (type display display)
	     (type resource-id property-id)
	     (type (or null resource-id) type-id))
    (multiple-value-bind (reply-format reply-type bytes-after data)
	(with-buffer-request-and-reply (display *x-getproperty* nil :sizes (8 32))
	     (((data boolean) delete-p)
	      (window window)
	      (resource-id property-id)
	      ((or null resource-id) type-id)
	      (card32 start)
	      (card32 (index- (or end 64000) start)))
	  (let ((reply-format (card8-get 1))
		(reply-type (card32-get 8))
		(bytes-after (card32-get 12))
		(nitems (card32-get 16)))
	    (values
	      reply-format
	      reply-type
	      bytes-after
	      (and (plusp nitems)
		   (ecase reply-format
		     (0  nil) ;; (make-sequence result-type 0) ;; Property not found.
		     (8  (sequence-get :result-type result-type :format card8
				       :length nitems :transform transform
				       :index *replysize*))
		     (16 (sequence-get :result-type result-type :format card16
				       :length nitems :transform transform
				       :index *replysize*))
		     (32 (sequence-get :result-type result-type :format card32
				       :length nitems :transform transform
				       :index *replysize*)))))))
      (values data
	      (and (plusp reply-type) (atom-name display reply-type))
	      reply-format
	      bytes-after))))

(defun rotate-properties (window properties &optional (delta 1))
  ;; Positive rotates left, negative rotates right (opposite of actual protocol request).
  (declare (type window window)
	   (type sequence properties) ;; sequence of xatom
	   (type int16 delta))
  (let* ((display (window-display window))
	 (length (length properties))
	 (sequence (make-array length)))
    (declare (type display display)
	     (type array-index length))
    (with-vector (sequence vector)
      ;; Atoms must be interned before the RotateProperties request
      ;; is started to allow InternAtom requests to be made.
      (dotimes (i length)
	(setf (aref sequence i) (intern-atom display (elt properties i))))
      (with-buffer-request (display *x-rotateproperties*)
	(window window)
	(card16 length)
	(int16 (- delta))
	((sequence :end length) sequence))))
  nil)

(defun list-properties (window &key (result-type 'list))
  (declare (type window window)
	   (type t result-type)) ;; a sequence type
  (declare (values (sequence keyword)))
  (let ((display (window-display window)))
    (multiple-value-bind (seq)
	(with-buffer-request-and-reply (display *x-listproperties* nil :sizes 16)
	     ((window window))
	  (values
	    (sequence-get :result-type result-type :length (card16-get 8)
			  :index *replysize*)))
      ;; lookup the atoms in the sequence
      (if (listp seq)
	  (do ((elt seq (cdr elt)))
	      ((endp elt) seq)
	    (setf (car elt) (atom-name display (car elt))))
	(dotimes (i (length seq) seq)
	  (setf (aref seq i) (atom-name display (aref seq i))))))))

(defun selection-owner (display selection)
  (declare (type display display)
	   (type xatom selection))
  (declare (values (or null window)))
  (let ((selection-id (intern-atom display selection)))
    (declare (type resource-id selection-id))
    (multiple-value-bind (window)
	(with-buffer-request-and-reply (display *x-getselectionowner* 12 :sizes 32)
	     ((resource-id selection-id))
	  (values
	    (resource-id-or-nil-get 8)))
      (and window (lookup-window display window)))))

(defun set-selection-owner (display selection owner &optional time)
  (declare (type display display)
	   (type xatom selection)
	   (type (or null window) owner)
	   (type timestamp time))
  (let ((selection-id (intern-atom display selection)))
    (declare (type resource-id selection-id))
    (with-buffer-request (display *x-setselectionowner*)
      ((or null window) owner)
      (resource-id selection-id)
      ((or null card32) time))
    owner))

(defsetf selection-owner (display selection &optional time) (owner)
  ;; A bit strange, but retains setf form.
  `(set-selection-owner ,display ,selection ,owner ,time))

(defun convert-selection (selection type requestor &optional property time)
  (declare (type xatom selection type)
	   (type window requestor)
	   (type (or null xatom) property)
	   (type timestamp time))
  (let* ((display (window-display requestor))
	 (selection-id (intern-atom display selection))
	 (type-id (intern-atom display type))
	 (property-id (and property (intern-atom display property))))
    (declare (type display display)
	     (type resource-id selection-id type-id)
	     (type (or null resource-id) property-id))
    (with-buffer-request (display *x-convertselection*)
      (window requestor)
      (resource-id selection-id type-id)
      ((or null resource-id) property-id)
      ((or null card32) time))))

(defun send-event (window event-key event-mask &rest args
		   &key propagate-p display &allow-other-keys)
  ;; Additional arguments depend on event-key, and are as specified further below
  ;; with declare-event, except that both resource-ids and resource objects are
  ;; accepted in the event components.  The display argument is only required if the
  ;; window is :pointer-window or :input-focus.
  (declare (type (or window (member :pointer-window :input-focus)) window)
	   (type event-key event-key)
	   (type (or null event-mask) event-mask)
	   (type boolean propagate-p)
	   (type (or null display) display)
	   (dynamic-extent args))
  (unless event-mask (setq event-mask 0))
  (unless display (setq display (window-display window)))
  (let ((internal-event-code (get-event-code event-key))
	(external-event-code (get-external-event-code display event-key)))
    (declare (type card8 internal-event-code external-event-code))
    ;; Ensure keyword atom-id's are cached
    (dolist (arg (cdr (assoc event-key '((:property-notify :atom)
					 (:selection-clear :selection)
					 (:selection-request :selection :target :property)
					 (:selection-notify :selection :target :property)
					 (:client-message :type))
			     :test #'eq)))
      (let ((keyword (getf args arg)))
	(intern-atom display keyword)))
    ;; Make the sendevent request
    (with-buffer-request (display *x-sendevent*)
      ((data boolean) propagate-p)
      (length 11) ;; 3 word request + 8 words for event = 11
      ((or (member :pointer-window :input-focus) window) window)
      (card32 (encode-event-mask event-mask))
      (card8 external-event-code)
      (progn
	(apply (svref *event-send-vector* internal-event-code) display args)
	(setf (buffer-boffset display) (index+ buffer-boffset 44))))))

(defun grab-pointer (window event-mask
		     &key owner-p sync-pointer-p sync-keyboard-p confine-to cursor time)
  (declare (type window window)
	   (type pointer-event-mask event-mask)
	   (type boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or null window) confine-to)
	   (type (or null cursor) cursor)
	   (type timestamp time))
  (declare (values grab-status))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display *x-grabpointer* nil :sizes 8)
	 (((data boolean) owner-p)
	  (window window)
	  (card16 (encode-pointer-event-mask event-mask))
	  (boolean (not sync-pointer-p) (not sync-keyboard-p))
	  ((or null window) confine-to)
	  ((or null cursor) cursor)
	  ((or null card32) time))
      (values
	(member8-get 1 :success :already-grabbed :invalid-time :not-viewable :frozen)))))

(defun ungrab-pointer (display &key time)
  (declare (type timestamp time))
  (with-buffer-request (display *x-ungrabpointer*)
    ((or null card32) time)))

(defun grab-button (window button event-mask
		    &key (modifiers 0)
			 owner-p sync-pointer-p sync-keyboard-p confine-to cursor)
  (declare (type window window)
	   (type (or (member :any) card8) button)
	   (type modifier-mask modifiers)
	   (type pointer-event-mask event-mask)
	   (type boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or null window) confine-to)
	   (type (or null cursor) cursor))
  (with-buffer-request ((window-display window) *x-grabbutton*)
    ((data boolean) owner-p)
    (window window)
    (card16 (encode-pointer-event-mask event-mask))
    (boolean (not sync-pointer-p) (not sync-keyboard-p))
    ((or null window) confine-to)
    ((or null cursor) cursor)
    (card8 (if (eq button :any) 0 button))
    (pad8 1)
    (card16 (encode-modifier-mask modifiers))))

(defun ungrab-button (window button &key (modifiers 0))
  (declare (type window window)
	   (type (or (member :any) card8) button)
	   (type modifier-mask modifiers))
  (with-buffer-request ((window-display window) *x-ungrabbutton*)
    (data (if (eq button :any) 0 button))
    (window window)
    (card16 (encode-modifier-mask modifiers))))

(defun change-active-pointer-grab (display event-mask &optional cursor time)
  (declare (type display display)
	   (type pointer-event-mask event-mask)
	   (type (or null cursor) cursor)
	   (type timestamp time))
  (with-buffer-request (display *x-changeactivepointergrab*)
    ((or null cursor) cursor)
    ((or null card32) time)
    (card16 (encode-pointer-event-mask event-mask))))

(defun grab-keyboard (window &key owner-p sync-pointer-p sync-keyboard-p time)
  (declare (type window window)
	   (type boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type timestamp time))
  (declare (values grab-status))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display *x-grabkeyboard* nil :sizes 8)
	 (((data boolean) owner-p)
	  (window window)
	  ((or null card32) time)
	  (boolean (not sync-pointer-p) (not sync-keyboard-p)))
      (values
	(member8-get 1 :success :already-grabbed :invalid-time :not-viewable :frozen)))))

(defun ungrab-keyboard (display &key time)
  (declare (type display display)
	   (type timestamp time))
  (with-buffer-request (display *x-ungrabkeyboard*)
    ((or null card32) time)))

(defun grab-key (window key &key (modifiers 0) owner-p sync-pointer-p sync-keyboard-p)
  (declare (type window window)
	   (type boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or (member :any) card8) key)
	   (type modifier-mask modifiers))
  (with-buffer-request ((window-display window) *x-grabkey*)
    ((data boolean) owner-p)
    (window window)
    (card16 (encode-modifier-mask modifiers))
    (card8 (if (eq key :any) 0 key))
    (boolean (not sync-pointer-p) (not sync-keyboard-p))))

(defun ungrab-key (window key &key (modifiers 0))
  (declare (type window window)
	   (type (or (member :any) card8) key)
	   (type modifier-mask modifiers))
  (with-buffer-request ((window-display window) *x-ungrabkey*)
    (data (if (eq key :any) 0 key))
    (window window)
    (card16 (encode-modifier-mask modifiers))))

(defun allow-events (display mode &optional time)
  (declare (type display display)
	   (type (member :async-pointer :sync-pointer :replay-pointer
			 :async-keyboard :sync-keyboard :replay-keyboard
			 :async-both :sync-both)
		 mode)
	   (type timestamp time))
  (with-buffer-request (display *x-allowevents*)
    ((data (member :async-pointer :sync-pointer :replay-pointer
		   :async-keyboard :sync-keyboard :replay-keyboard
		   :async-both :sync-both))
     mode)
    ((or null card32) time)))

(defun grab-server (display)
  (declare (type display display))
  (with-buffer-request (display *x-grabserver*)))

(defun ungrab-server (display)
  (with-buffer-request (display *x-ungrabserver*)))

(defmacro with-server-grabbed ((display) &body body)
  ;; The body is not surrounded by a with-display.
  (let ((disp (if (symbolp display) display (gensym))))
    `(let ((,disp ,display))
       (declare (type display ,disp))
       (unwind-protect
	   (progn
	     (grab-server ,disp)
	     ,@body)
	 (ungrab-server ,disp)))))

(defun query-pointer (window)
  (declare (type window window))
  (declare (values x y same-screen-p child mask root-x root-y root))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display *x-querypointer* 26 :sizes (8 16 32))
	 ((window window))
      (values
	(int16-get 20)
	(int16-get 22)
	(boolean-get 1)
	(or-get 12 null window)
	(card16-get 24)
	(int16-get 16)
	(int16-get 18)
	(window-get 8)))))

(defun pointer-position (window)
  (declare (type window window))
  (declare (values x y same-screen-p))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display *x-querypointer* 24 :sizes (8 16))
	 ((window window))
      (values
	(int16-get 20)
	(int16-get 22)
	(boolean-get 1)))))

(defun global-pointer-position (display)
  (declare (type display display))
  (declare (values root-x root-y root))
  (with-buffer-request-and-reply (display *x-querypointer* 20 :sizes (16 32))
       ((window (screen-root (first (display-roots display)))))
    (values
      (int16-get 16)
      (int16-get 18)
      (window-get 8))))

(defun motion-events (window &key start stop (result-type 'list))
  (declare (type window window)
	   (type timestamp start stop)
	   (type t result-type)) ;; a type specifier
  (declare (values (repeat-seq (integer x) (integer y) (timestamp time))))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display *x-getmotionevents* nil :sizes 32)
	 ((window window)
	  ((or null card32) start stop))
      (values
	(sequence-get :result-type result-type :length (index* (card32-get 8) 3)
		      :index *replysize*)))))

(defun translate-coordinates (src src-x src-y dst)
  ;; Returns NIL when not on the same screen
  (declare (type window src)
	   (type int16 src-x src-y)
	   (type window dst))
  (declare (values dst-x dst-y child))
  (let ((display (window-display src)))
    (with-buffer-request-and-reply (display *x-translatecoords* 16 :sizes (8 16 32))
	 ((window src dst)
	  (int16 src-x src-y))
      (and (boolean-get 1)
	   (values
	     (int16-get 12)
	     (int16-get 14)
	     (or-get 8 null window))))))

(defun warp-pointer (dst dst-x dst-y)
  (declare (type window dst)
	   (type int16 dst-x dst-y))
  (with-buffer-request ((window-display dst) *x-warppointer*)
    (resource-id 0) ;; None
    (window dst)
    (int16 0 0)
    (card16 0 0)
    (int16 dst-x dst-y)))

(defun warp-pointer-relative (display x-off y-off)
  (declare (type display display)
	   (type int16 x-off y-off))
  (with-buffer-request (display *x-warppointer*)
    (resource-id 0) ;; None
    (resource-id 0) ;; None
    (int16 0 0)
    (card16 0 0)
    (int16 x-off y-off)))

(defun warp-pointer-if-inside (dst dst-x dst-y src src-x src-y
			       &optional src-width src-height)
  ;; Passing in a zero src-width or src-height is a no-op.
  ;; A null src-width or src-height translates into a zero value in the protocol request.
  (declare (type window dst src)
	   (type int16 dst-x dst-y src-x src-y)
	   (type (or null card16) src-width src-height))
  (unless (or (eql src-width 0) (eql src-height 0))
    (with-buffer-request ((window-display dst) *x-warppointer*)
      (window src dst)
      (int16 src-x src-y)
      (card16 (or src-width 0) (or src-height 0))
      (int16 dst-x dst-y))))

(defun warp-pointer-relative-if-inside (x-off y-off src src-x src-y
					&optional src-width src-height)
  ;; Passing in a zero src-width or src-height is a no-op.
  ;; A null src-width or src-height translates into a zero value in the protocol request.
  (declare (type window src)
	   (type int16 x-off y-off src-x src-y)
	   (type (or null card16) src-width src-height))
  (unless (or (eql src-width 0) (eql src-height 0))
    (with-buffer-request ((window-display src) *x-warppointer*)
      (window src)
      (resource-id 0) ;; None
      (int16 src-x src-y)
      (card16 (or src-width 0) (or src-height 0))
      (int16 x-off y-off))))
