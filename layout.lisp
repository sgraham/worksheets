(in-package :learnr)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(defgeneric render (layout-obj canvas))

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

(defmethod render ((obj layout-background) canvas)
  (apply #'set-source-rgb (colour obj))
  (paint))


(defmethod render ((obj layout-text) canvas)
  (apply #'set-source-rgb (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 12)
  (move-to 0 0)
  (show-text (text obj)))

#|
(layout-horiz
  (layout-text "20 + 12 = ")
  (layout-group
    (layout-line 0 -.1 2 -.1)
    (layout-hcentre-text "32")))
|#

