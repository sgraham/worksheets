(in-package :learnr)

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

(defun generate-and-layout (qd pagewidth)
  (let* ((props (dotimes (bail 1000)
                 (let ((prop-values (mapcar #'generate (properties qd))))
                   (when (funcall (constraints qd) prop-values)
                     (return prop-values)))))
         (answer (funcall (answer qd) props)))
    (print props)
    (print answer)))


;;;
;;; layout object
;;;

(defclass layout-text ()
  ((text :initarg :text :accessor text)
   (color :initarg :color :accessor color)))

(defclass layout-group ()
  ((items :initarg :items :initform (list) :accessor items)))

;;;
;;; cairo rendering for layout objects
;;;
#|(defgeneric cairo-render (layout-object context)
  (:documentation "render layout object into given cairo context"))|#

;(defmethod cairo-render ((obj layout-text) context)

(defun draw-test ()
  (set-source-rgb 1 1 1)
  (paint)
  (set-source-rgb .2 .2 1)
  (move-to 200 0)
  (line-to 0 200)
  (stroke)
  (set-source-rgb 0 0 0)
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 12)
  (multiple-value-bind (x-bearing y-bearing text-width text-height x-adv y-adv)
      (text-extents "Some text")
    (print x-bearing)
    (print y-bearing)
    (print text-width)
    (print text-height)
    (print x-adv)
    (print y-adv))
  (move-to 95 95)
  (reset-trans-matrix)
  (rotate (deg-to-rad -45))
  (show-text "Some text"))

(defparameter *letter-width* 612)
(defparameter *letter-height* 792)
  
(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
  (draw-test))

(setf *context* (create-pdf-context "example.pdf" *letter-width* *letter-height*))
(draw-test)
(destroy *context*)

