(in-package :learnr)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))


(defgeneric render (layout-obj canvas))

(defclass layout-background ()
  ((colour :initarg :colour
           :reader colour)))

(defmethod render ((obj layout-background) canvas)
  (apply #'set-source-rgb (colour obj))
  (paint))


(defclass layout-text ()
  ((text :initarg :text
         :reader text)
   (colour :initarg :colour
           :reader colour)))

(defun layout-text (text &optional (colour +black+))
  (make-instance 'layout-text :text text :colour colour))
(defmethod render ((obj layout-text) canvas)
  (apply #'set-source-rgb (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 12)
  (move-to 0 0)
  (show-text (text obj)))
(defmethod print-object ((obj layout-text) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (text colour) obj
      (format stream "text: ~a, colour: ~a"
	      text colour))))


;(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
;  (let* ((bg (make-instance 'layout-background :colour '(1 1 1)))
;         (x (make-instance 'layout-text :text "This is some textgpq" :colour '(0 0 1))))
;        (progn
;         (render bg nil)
;         (render x nil))))

#|
(layout-horiz
  (layout-text "20 + 12 = ")
  (layout-group
    (layout-line 0 -.1 2 -.1)
    (layout-hcentre-text "32")))
|#

(defclass layout-horiz ()
  ((children :initarg :children
             :reader children)
   (spacing :initarg :spacing
            :reader spacing)))
(defun layout-horiz (&rest children) ; todo; spacing
  (make-instance 'layout-horiz :spacing 0.0 :children children))
(defmethod print-object ((obj layout-horiz) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (spacing children) obj
      (format stream "spacing: ~a, children: ~a"
	      spacing children))))


(defclass layout-group ()
  ((children :initarg :children
             :reader children)))
(defun layout-group (&rest children)
  (make-instance 'layout-group :children children))
(defmethod print-object ((obj layout-group) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (children) obj
      (format stream "children: ~a"
	      children))))

(defclass layout-line ()
  ((x0 :initarg :x0
       :reader x0)
   (y0 :initarg :y0
       :reader y0)
   (x1 :initarg :x1
       :reader x1)
   (y1 :initarg :y1
       :reader y1)))
(defun layout-line (x0 y0 x1 y1)
  (make-instance 'layout-line :x0 x0 :y0 y0 :x1 x1 :y1 y1))
(defmethod print-object ((obj layout-line) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (x0 y0 x1 y1) obj
      (format stream "p0: ~a ~a, p1: ~a ~a"
	      x0 y0 x1 y1))))


(defclass layout-hcentre-text (layout-text)
  ())

(defun layout-hcentre-text (text colour)
  (make-instance 'layout-hcentre-text :text text :colour colour))

