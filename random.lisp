(in-package :learnr)

;;;
;;; random numbers
;;;

(defgeneric get-random (rng min max)
  (:documentation "get a random number from rng in [min..max]"))

(defstruct prng state)
(defmethod get-random ((rng prng) min max)
  (+ min
     (random (1+ (- max min))
             (prng-state rng))))

(defstruct mock-rng state)
(defmethod get-random ((rng mock-rng) min max)
  (let ((val (pop (mock-rng-state rng))))
    (assert (and (<= min val) (>= max val)))
    val))
