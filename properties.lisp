(in-package :learnr)

(defstruct property
  name value)

(defgeneric generate (property vals))

(defstruct (property-int (:include property))
  min
  max
  minmin
  maxmax)

(defmethod generate ((prop property-int) vals)
  (let ((num (random-range (property-int-min prop) (property-int-max prop))))
    (setf (gethash (property-name prop) vals) num)))
