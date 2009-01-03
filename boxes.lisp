(in-package :learnr)

(defclass box ()
  ((width :accessor width :initform 0 :initarg :width)
   (height :accessor height :initform 0 :initarg :height)
   (depth :accessor depth :initform 0 :initarg :depth)))
