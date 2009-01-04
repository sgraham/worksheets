(asdf:oos 'asdf:load-op :cl-cairo2)
(asdf:oos 'asdf:load-op :cl-colors)
(asdf:oos 'asdf:load-op :fiveam)

(defpackage :learnr
  (:use :common-lisp
        :cl-cairo2
        :cl-colors
        :it.bese.FiveAM))

(in-package :learnr)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(defmacro with-pdf-file ((filename width height) &body body)
  "Execute the body with context bound to a newly created pdf
  file, and close it after executing body."
  `(let* ((*context* (create-pdf-context ,filename ,width ,height)))
         (unwind-protect (progn ,@body)
                         (destroy *context*))))
;;;
;;;
;;; random numbers
;;;
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


;;;
;;;
;;; properties
;;;
;;;

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


;;;
;;;
;;; layout
;;;
;;;
(defgeneric render (layout-obj &optional bbox))
(defgeneric bbox (layout-obj))

(defun just-names (items)
  (mapcar #'(lambda (item)
              (if (listp item)
                (car item)
                item))
          items))

(defmacro deflayoutobj (name &rest items)
  `(progn
     (defclass ,name ()
       (,@(mapcar #'(lambda (item)
                      (let ((ia (intern (symbol-name item) "KEYWORD")))
                        `(,item :initarg ,ia
                                :reader ,item)))
                  (just-names items))))
     (defmethod print-object ((obj ,name) stream)
       (print-unreadable-object (obj stream :type t)
         (with-slots ,(just-names items) obj
           ,@(mapcar #'(lambda (item)
                         `(progn
                            (princ ,item stream)
                            (princ " " stream)))
                     (just-names items)))))
     ; todo; only optional for things that have default?
     (defun ,name (&optional ,@items)
       (make-instance ',name
                      ,@(mapcan #'(lambda (item)
                                    (let ((ikw (intern (symbol-name item) "KEYWORD")))
                                      `(,ikw ,item)))
                                (just-names items))))))

(deflayoutobj layout-background colour)
(deflayoutobj layout-text text (colour +black+))
(deflayoutobj layout-horiz children spacing)
(deflayoutobj layout-group children)
(deflayoutobj layout-line x0 y0 x1 y1)
(deflayoutobj layout-hcentre-text text colour)

; todo; merge render/bbox with a macro that walks and does show/extents?

(defmethod render ((obj layout-background) &optional bbox)
  (set-source-color (colour obj))
  (paint))

(defmethod render ((obj layout-text) &optional bbox)
  (set-source-color (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 14)
  (move-to 0 0)
  (show-text (text obj)))

(defmethod bbox ((obj layout-text))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 14)
  (multiple-value-bind (x-bearing y-bearing text-width text-height xadv yadv)
    (text-extents (text obj))
    (list xadv yadv)))

(defmethod render ((obj layout-group) &optional bbox)
  (let ((groupbbox (bbox obj)))
    (dolist (cmd (children obj))
      (render cmd groupbbox))))

(defmethod bbox ((obj layout-group))
  (let ((bboxes (mapcar #'(lambda (child) (bbox child))
                        (children obj))))
    (list (reduce #'max bboxes :key #'car)
          (reduce #'max bboxes :key #'cadr))))

; todo; rewrite not in pascal
(defmethod render ((obj layout-horiz) &optional bbox)
  (let ((x 0))
    (save)
    (dolist (cmd (children obj))
      (render cmd)
      (setf x (+ x (first (bbox cmd))))
      (translate x 0))
    (restore)))


(defmethod render ((obj layout-line) &optional bbox)
  (set-source-color +black+)
  (move-to (x0 obj) (y0 obj))
  (line-to (x1 obj) (y1 obj))
  (stroke))

(defmethod bbox ((obj layout-line))
  (move-to (x0 obj) (y0 obj))
  (line-to (x1 obj) (y1 obj))
  (list (- (x1 obj) (x0 obj)) (- (y1 obj) (y0 obj))))

(defmethod render ((obj layout-hcentre-text) &optional bbox)
  (set-source-color (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 14)
  (let* ((te (multiple-value-list (text-extents (text obj))))
         (xadv (fifth te)))
    (move-to (/ (- (car bbox) xadv) 2) 0))
  (show-text (text obj)))

(defmethod bbox ((obj layout-hcentre-text))
  '(0 0))

;;;
;;;
;;; question data
;;;
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

(defparameter *question-db* (make-hash-table :test #'equal))

(defun add-question-description (qd)
  (setf (gethash (concatenate 'string (qd-group qd) "/" (qd-title qd)) *question-db*)
        qd))

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
                   (list 
                     (layout-text (format nil "~a + ~a = " a b))
                     answer)))))

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
                   (list (layout-text (format nil "~a \div ~a = " a b))
                         answer)))))

(defun generate-question (rng qd answered)
  (let* ((propvals (dotimes (bail 1000) ; todo
                     (let ((possiblevals (mapcar
                                           #'(lambda (p) (generate rng p))
                                           (qd-properties qd))))
                       (if (qd-constraints qd)
                         (when (apply (qd-constraints qd) possiblevals)
                           (return possiblevals))
                         (return possiblevals)))))
         (answerval (write-to-string (apply (qd-answer qd) propvals)))
         (answer-layout (if answered 
                          (layout-group
                            (list (layout-line 0 5 72 5)
                                  (layout-hcentre-text answerval +red+)))
                          (layout-line 0 5 72 5))))
    (apply (qd-display qd) (append propvals (list answer-layout)))))

(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
  (render (layout-background +white+))
  (let* ((rng (make-prng :state (make-random-state t)))
         (qd (gethash "Basic Number Operations/Addition" *question-db*))
         (allgen (loop for i to 11 collect (generate-question rng qd t))))
    (reset-trans-matrix)
    (translate 0 40)
    (loop for qlayout in allgen do
          (render qlayout)
          (translate (/ *letter-width* 3) 0))))

(defparameter *letter-width* 612)
(defparameter *letter-height* 792)
  
(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
               (translate 0 100)
               (dolist (obj (list
                              (layout-background +white+)
                              (layout-horiz
                                (list (layout-text "12 + 20 = x⁴")
                                      (layout-group
                                        (list (layout-line 0 5 72 5)
                                              (layout-hcentre-text "32" +red+)))))))
                 (render obj)))

;;;
;;;
;;; document
;;;
;;;

(defclass page ()
  ((sets :initform (list)
         :accessor sets)))

(defclass document ()
  ((pages :initform (list)
          :reader pages)
   (width :initarg :width
          :initform 8.5
          :reader width)
   (height :initarg :height
           :initform 11
           :reader height)))

;;;
;;; cairo rendering for layout objects
;;;
#|(defgeneric cairo-render (layout-object context)
              (:documentation "render layout object into given cairo context"))|#

;(defmethod cairo-render ((obj layout-text) context)

;(defun draw-test ()
;  (set-source-rgb 1 1 1)
;  (paint)
;  (set-source-rgb .2 .2 1)
;  (move-to 200 0)
;  (line-to 0 200)
;  (stroke)
;  (set-source-rgb 0 0 0)
;  (select-font-face "Segoe UI" :normal :normal)
;  (set-font-size 12)
;  (multiple-value-bind (x-bearing y-bearing text-width text-height x-adv y-adv)
;      (text-extents "Some text")
;    (print x-bearing)
;    (print y-bearing)
;    (print text-width)
;    (print text-height)
;    (print x-adv)
;    (print y-adv))
;  (move-to 95 95)
;  (reset-trans-matrix)
;  (rotate (deg-to-rad -45))
;  (show-text "Some text"))
;
;; this is pixels/points (which are the same for us)
;(defparameter *letter-width* 612)
;(defparameter *letter-height* 792)
;  
;(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
;  (draw-test))
;
;
;(with-pdf-file ("example.pdf" *letter-width* *letter-height*)
;  (draw-test))
