(in-package :learnr)

(defstruct property
  name)

(defgeneric generate (rng property))

(defstruct (property-int (:include property))
  min
  max
  minmin
  maxmax)

(defmethod generate (rng (prop property-int))
  (let ((num (get-random rng (property-int-min prop) (property-int-max prop))))
       num))
