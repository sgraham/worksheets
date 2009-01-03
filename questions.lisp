(in-package :learnr)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

;;;
;;; question data
;;;
(defstruct (question-description (:conc-name qd-))
  group
  title
  properties
  default-instr
  default-count
  default-cols
  answer
  constraints
  display)

(defparameter *question-db* nil)

(defun add-question-description (qd) (push qd *question-db*))

(add-question-description
  (make-question-description
                 :group "Basic Number Operations" 
                 :title "Addition"
                 :properties (list (make-property-int :name "Integer 1" :min -20 :max 20)
                                   (make-property-int :name "Integer 2" :min -20 :max 20))
                 :default-instr "Solve by adding."
                 :default-count 12
                 :default-cols 3
                 :answer #'(lambda (a b) (+ a b))
                 :display #'(lambda (a b answer)
                                    (layout-horiz
                                     (layout-text (format nil "~a + ~a = " a b))
                                     answer))))

(add-question-description
  (make-question-description
                 :group "Basic Number Operations" 
                 :title "Division (Integral Answer)"
                 :properties (list (make-property-int :name "Dividend" :min 10 :max 100)
                                   (make-property-int :name "Divisor" :min 1 :max 10))
                 :default-instr "Solve by dividing."
                 :default-count 12
                 :default-cols 3
                 :answer #'(lambda (a b) (/ a b))
                 :constraints #'(lambda (a b)
                                  (and (not (eq b 0))
                                       (eq (/ a b) (floor a b))))
                 :display #'(lambda (a b answer)
                                    (layout-horiz
                                     (layout-text (format nil "~a \div ~a = " a b))
                                     answer))))

(defun generate-question (rng qd answered)
  (let* ((propvals (dotimes (bail 1000) ; todo
                            (let ((possiblevals (mapcar
                                                 #'(lambda (p) (generate rng p))
                                                 (qd-properties qd))))
                                 (if (qd-constraints qd)
                                     (when (apply (qd-constraints qd) possiblevals)
                                           (return possiblevals))
                                     (return possiblevals)))))
         (answerval (apply (qd-answer qd) propvals))
         (answer-layout (if answered 
                            (layout-group
                             (layout-line 0 -0.1 2 -0.1)
                             (layout-hcentre-text answerval +red+))
                            (layout-line 0 -0.1 2 -0.1))))
    (apply (qd-display qd) (append propvals (list answer-layout)))))

(let ((rng (make-mock-rng :state '(12 20))))
  (generate-question rng (second *question-db*) t))
