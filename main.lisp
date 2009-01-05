(asdf:oos 'asdf:load-op :cl-cairo2)
(asdf:oos 'asdf:load-op :cl-colors)
(asdf:oos 'asdf:load-op :iterate)
(asdf:oos 'asdf:load-op :cl-who)
(asdf:oos 'asdf:load-op :cl-base64)
(asdf:oos 'asdf:load-op :ironclad)

(defpackage :learnr
  (:use :common-lisp
        :cl-cairo2
        :cl-colors
        :cl-who
        :iterate))

(in-package :learnr)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(defmacro with-pdf-file ((filename width height) &body body)
  "Execute the body with context bound to a newly created pdf
  file, and close it after executing body."
  `(let* ((*context* (create-pdf-context ,filename ,width ,height)))
     (unwind-protect (progn ,@body)
       (destroy *context*))))

(defmacro with-null-pdf-context (&body body)
  `(let* ((*context* (create-pdf-context "/dev/null" *letter-width* *letter-height*)))
     (unwind-protect (progn ,@body)
       (destroy *context*))))

(with-pdf-file ((cffi:null-pointer) 500 500)
  (move-to 100 100)
  (line-to 200 200)
  (stroke))

(defparameter *letter-width* 612)
(defparameter *letter-height* 792)


;;;
;;;
;;; utilities
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

(defun get-id ()
  "generates a url-embeddable id in an overly complicated way"
  (string-right-trim "="
                     (cl-base64:usb8-array-to-base64-string
                       (ironclad:digest-sequence
                         :sha1
                         (ironclad:ascii-string-to-byte-array
                           (format nil "~a:~a"
                                   (random 10000000)
                                   (get-internal-real-time)))))))



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
(defgeneric render (layout-obj &optional extents))
(defgeneric extents (layout-obj))

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
(deflayoutobj layout-hcentre children)

; todo; merge render/bbox with a macro that walks and does show/extents?

(defmethod render ((obj layout-background) &optional extents)
  (set-source-color (colour obj))
  (paint))

#|
(macroexpand-1
'(deflayoutobjrender layout-text
  (set-source-color (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 13)
  (move-to 0 0)
  (handle-text (text obj))))

(defun sub-as-render (body)
  body)

(defun sub-as-bbox (body)
  "handle-text -> text-extents"
  (mapcan #'(lambda (x) (and (not (eq (car x) 'set-source-color)) (list x)))
          body))

(defmacro deflayoutobjrender (lotype &body body)
  `(progn
     (defmethod render ((obj ,lotype) &optional extents)
       ,@(sub-as-render body))
     (defmethod bbox ((obj ,lotype))
       ,@(sub-as-bbox body))))
|#

(defmethod render ((obj layout-text) &optional extents)
  (set-source-color (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 13)
  (move-to 0 0)
  (show-text (text obj)))

(defmethod extents ((obj layout-text))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 13)
  (text-path (text obj))
  (multiple-value-list (path-extents)))

(with-null-pdf-context
  (extents (layout-text "Thsi is stuff")))

(defmethod render ((obj layout-group) &optional extents)
  (let ((groupextents (extents obj)))
    (dolist (cmd (children obj))
      (render cmd groupextents))))

(defun extents-union (a b)
  (list (min (first a) (first b))
        (min (second a) (second b))
        (max (third a) (third b))
        (max (fourth a) (fourth b))))
(defun extents-width (ext)
  (- (third ext) (first ext)))
(defun extents-height (ext)
  (- (fourth ext) (second ext)))

(defmethod extents ((obj layout-group))
  (new-path)
  (let ((childextents (mapcar #'extents (children obj))))
    (reduce #'extents-union childextents)))

#|(with-null-pdf-context
  (extents (layout-group (list (layout-text "stuffWp'`p") (layout-text "zzzzzzzzzzzzzzzzz")))))|#

#|(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
  (new-path)
  (render (layout-background +white+))
  (print (extents (layout-line 0 5 72 5)))
  (translate 0 100)
  (print (extents (layout-hcentre
            (list (layout-line 0 5 72 5)
                  (layout-text "weewaa" +red+)))))
  (render (layout-hcentre
            (list (layout-line 0 5 72 5)
                  (layout-text "weewaa" +red+)))))|#

; todo; rewrite not in pascal
(defmethod render ((obj layout-horiz) &optional extents)
  (let ((x 0))
    (save)
    (dolist (cmd (children obj))
      (render cmd)
      (setf x (+ x (first (bbox cmd))))
      (translate x 0))
    (restore)))

(defmethod bbox ((obj layout-horiz))
  (let ((bboxes (mapcar #'(lambda (child) (bbox child))
                        (children obj))))
    (list (reduce #'max bboxes :key #'car)
          (reduce #'max bboxes :key #'cadr))))

(defmethod render ((obj layout-line) &optional extents)
  (new-path)
  (set-source-color +black+)
  (move-to (x0 obj) (y0 obj))
  (line-to (x1 obj) (y1 obj))
  (stroke))

(defmethod extents ((obj layout-line))
  (move-to (x0 obj) (y0 obj))
  (line-to (x1 obj) (y1 obj))
  (multiple-value-list (path-extents)))

#|(defmethod render ((obj layout-hcentre-text) &optional extents)
  (set-source-color (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 13)
  (if (null extents)
    (move-to 0 0)
    (let ((text-width (extents-width (extents obj))))
      (print text-width)
      (print (extents-width extents))
      (move-to (/ (- (extents-width extents) text-width) 2) 0)))
  (show-text (text obj)))|#

#|(defmethod extents ((obj layout-hcentre-text))
  (new-path)
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 13)
  (text-path (text obj))
  (multiple-value-list (path-extents)))|#

(defmethod render ((obj layout-hcentre) &optional extents)
  (let ((allwidth (extents-width (extents obj))))
    (save)
    (iter (for c in (children obj))
          (translate (/ (- allwidth
                           (extents-width (extents c)))
                        2)
                     0)
          (render c))
    (restore)))

(defmethod extents ((obj layout-hcentre))
  (reduce #'extents-union (mapcar #'extents (children obj))))


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
                          (layout-hcentre
                            (list (layout-line 0 5 72 5)
                                  (layout-text answerval +red+)))
                          (layout-line 0 5 72 5))))
    (apply (qd-display qd) (append propvals (list answer-layout)))))

#|(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
  (render (layout-background +white+))
  (let* ((rng (make-prng :state (make-random-state t)))
         (qd (gethash "Basic Number Operations/Addition" *question-db*))
         (allgen (iter (for i to 11)
                       (collect (generate-question rng qd t)))))
    (reset-trans-matrix)
    (translate 0 40)
    (iter (for qlayout in allgen)
          (render qlayout)
          (translate (/ *letter-width* 3) 0))))|#


;;;
;;;
;;; document
;;;
;;;

(defclass question-set ()
  ((question-description :initarg :question-description
                         :reader question-description)
   (cols :initarg :cols
         :accessor cols)
   (instr :initarg :instr
          :accessor instr)
   (questions :initarg :questions
              :accessor questions)))

(defclass hrow ()
  ((items :initarg :items
          :accessor items)
   (bboxes :initarg :bboxes
           :accessor bboxes)))

(defun generate-set (qd answered &key (count (qd-default-count qd)) (cols (qd-default-cols qd)) (instr (qd-default-instr qd)))
  (let* ((rng (make-prng :state (make-random-state t))))
    (make-instance 'question-set
                   :question-description qd
                   :cols cols
                   :instr instr
                   :questions (iter (for i to (1- count))
                                    (collect (generate-question rng qd answered))))))


(defun layout-set (qset)
  (let ((bboxes (mapcar #'bbox (questions qset))))
    bboxes))

(defvar *testset* (generate-set (gethash "Basic Number Operations/Addition" *question-db*) t))

;
;(with-null-pdf-context
;  (layout-set *testset*))


(defclass document ()
  ((hrows :initform (list)
          :accessor hrows)
   (sets :initform (list)
         :accessor sets)))

(defun add-set (doc qset) (push (sets doc) qset))

(defun layout-sets (doc)
  ; bbox for each question
  ; hrow based cols setting
  ; (bbox hrow) to stack onto pages
  (with-pdf-file (nil *letter-width* *letter-height*)
    (let* ((hrow-groups (mapcar #'layout-set (sets doc)))
           (hrows (concatenate 'list hrow-groups))))))



;(add-set *doc* (generate-set (gethash "Basic Number Operations/Addition" *question-db*) t))
;(layout-sets *doc*)

"""
- add set to document
- edit set properties (num columns, num items, field values in description)
    - regen (shouldn't on num columns maybe)
- move set around in document
- delete set
- question number, instructions, score for tests

- for laying out on pages:
    - use null pdf context
    - generate set of hrows
    - pack hrows into pages, overflowing to new pages as needed
    - render each hrow as png, insert ------ for page break
        - ------ can be shadowed end of page, start of next

- test by gen of trivial html+png

- adapt math from cl-typesetting maybe? (fractions, superscripts, etc.)

- question types
    - title 'set' (name, date)
    - hrule
    - custom type-in-text question

"""


  
#|(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
               (translate 0 100)
               (dolist (obj (list
                              (layout-background +white+)
                              (layout-horiz
                                (list (layout-text "12 + 20 = x⁴")
                                      (layout-group
                                        (list (layout-line 0 5 72 5)
                                              (layout-hcentre-text "32" +red+)))))))
                 (render obj)))|#

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
