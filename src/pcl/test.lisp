;;; Stuff from the MOP book

(defclass rectangle ()
  ((height :initarg :start-height
	   :initform 5
	   :accessor rectangle-height)
   (width :initarg :start-width
	  :initform 8
	  :accessor rectangle-width)))

(defclass color-rectangle (rectangle)
  ((color :initform 'red
	  :initarg :color
	  :accessor color)
   (clearp :initform nil
	   :initarg :clearp
	   :accessor clearp)
   (height :initform 10)))

(defclass color-mixin ()
  ((color :initform 'red
	  :initarg :color
	  :accessor color)))

(defclass color-rectangle-2 (color-mixin rectangle)
  ((clearp :initform nil
	   :initarg :clearp
	   :accessor clearp)
   (height :initform 10)))

(defparameter r1 (make-instance 'rectangle
				:start-height 50 :start-width 100))

(defparameter r2 (make-instance 'color-rectangle
				:start-height 50 :start-width 100))

(defparameter r3 (make-instance 'color-rectangle-2
				:start-height 50 :start-width 100))

(defgeneric paint (shape medium))

(defmethod paint ((shape rectangle) medium)
  :m1)

(defmethod paint ((shape color-rectangle) medium)
  :m2)

(defmethod paint ((shape color-rectangle-2) medium)
  :m3)

(defmethod paint :before ((shape color-mixin) medium)
  (print :before-method))

