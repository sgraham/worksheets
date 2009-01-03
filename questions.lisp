(in-package :learnr)

;;;
;;; question data
;;;
(defstruct question-description
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
                              (list (format nil "~a + ~a = " a b)
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
                              (list (format nil "~a \div ~a = " a b)
                                    answer))))
