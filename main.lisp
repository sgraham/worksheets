(asdf:oos 'asdf:load-op :cl-cairo2)
(asdf:oos 'asdf:load-op :cl-colors)
(asdf:oos 'asdf:load-op :iterate)
(asdf:oos 'asdf:load-op :cl-who)
(asdf:oos 'asdf:load-op :cl-base64)
(asdf:oos 'asdf:load-op :ironclad)
(asdf:oos 'asdf:load-op :symbolicweb)

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
(defparameter *margin* 36)
(defparameter *page-width* (- *letter-width* *margin* *margin*))
(defparameter *page-height* (- *letter-height* *margin* *margin*))


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
(defgeneric render (layout-obj))
(defgeneric extents (layout-obj))

(defclass layout-object () ())

(defgeneric horiz-advance (layout-obj))

(defmethod horiz-advance ((obj layout-object))
  (extents-width (extents obj)))


(defun just-names (items)
  (mapcar #'(lambda (item)
              (if (listp item)
                (car item)
                item))
          items))

(defmacro deflayoutobj (name &rest items)
  `(progn
     (defclass ,name (layout-object)
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
(deflayoutobj layout-line x0 y0 x1 y1)
(deflayoutobj layout-hcentre children)
(deflayoutobj layout-columns num children)

; todo; merge render/bbox with a macro that walks and does show/extents?

(defmethod render ((obj layout-background))
  (set-source-color (colour obj))
  (paint))

(defmethod render ((obj layout-text))
  (set-source-color (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 13)
  (move-to 0 0)
  (show-text (text obj)))

(defmethod extents ((obj layout-text))
  (new-path)
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 13)
  (text-path (text obj))
  (multiple-value-list (path-extents)))

(defmethod horiz-advance ((obj layout-text))
  (set-source-color (colour obj))
  (select-font-face "Segoe UI" :normal :normal)
  (set-font-size 13)
  (move-to 0 0)
  (multiple-value-bind (x-bearing y-bearing width height xadv yadv) (text-extents (text obj))
    (print '(x-bearing y-bearing width height xadv yadv))
    xadv))

(defun extents-union (a &optional b)
  (if (null b)
    a
    (list (min (first a) (first b))
          (min (second a) (second b))
          (max (third a) (third b))
          (max (fourth a) (fourth b)))))
(defun extents-width (ext)
  (- (third ext) (first ext)))
(defun extents-height (ext)
  (- (fourth ext) (second ext)))

(defmethod render ((obj layout-horiz))
  (let ((x 0))
    (iter (for c in (children obj))
          (save)
          (translate x 0)
          (render c)
          (setf x (+ x (horiz-advance c)))
          (restore))))

(defmethod extents ((obj layout-horiz))
  (let* ((childextents (mapcar #'extents (children obj)))
         (totalwidth (reduce #'+ (append (mapcar #'extents-width childextents)
                                         (mapcar #'first childextents))))
         (unioned (reduce #'extents-union childextents))
         (leftmostpoint (first (first childextents))))
    (list leftmostpoint
          (second unioned)
          (+ leftmostpoint totalwidth)
          (fourth unioned))))

(defmethod render ((obj layout-line))
  (new-path)
  (set-source-color +black+)
  (move-to (x0 obj) (y0 obj))
  (line-to (x1 obj) (y1 obj))
  (stroke))

(defmethod extents ((obj layout-line))
  (new-path)
  (move-to (x0 obj) (y0 obj))
  (line-to (x1 obj) (y1 obj))
  (multiple-value-list (path-extents)))

(defmethod render ((obj layout-hcentre))
  (let ((allwidth (extents-width (extents obj))))
    (iter (for c in (children obj))
          (save)
          (translate (/ (- allwidth
                           (extents-width (extents c)))
                        2)
                     0)
          (render c)
          (restore))))

(defmethod extents ((obj layout-hcentre))
  (reduce #'extents-union (mapcar #'extents (children obj))))

(defun into-n-sized-chunks (children n)
  (assert (> n 0))
  (if (<= (length children) n)
    (list children)
    (cons (subseq children 0 n)
          (into-n-sized-chunks (subseq children n) n))))

(defun get-row-heights (obj)
  (let* ((chunked (into-n-sized-chunks (children obj) (num obj)))
         (chunked-extents (mapcar #'(lambda (x) (mapcar #'extents x)) chunked))
         (row-extents (mapcar #'(lambda (x) (reduce #'extents-union x)) chunked-extents)) ; hmm, union, or max? I think union because of initial advance
         (row-heights (mapcar #'extents-height row-extents)))
    row-heights))

(defun get-col-offsets (num width)
  (iter (for i from 0 below width by (/ width num))
        (collect i)))

(defun row-offsets-from-heights (row-heights)
  (let ((summed (iter (for i in row-heights)
                      (sum i into x)
                      (collect x))))
    (cons 0 (subseq summed 0 (1- (length summed))))))


(defmethod render ((obj layout-columns))
  (let* ((row-heights (get-row-heights obj))
         (row-offsets (row-offsets-from-heights row-heights))
         (col-offsets (get-col-offsets (num obj) *page-width*)))
    (iter (for child in (children obj))
          (for i from 0)
          (multiple-value-bind (row col) (floor i (num obj))
            (save)
            (translate (nth col col-offsets) (nth row row-offsets))
            (render child)
            (restore)))))


(defmethod extents ((obj layout-columns))
  (let* ((row-heights (get-row-heights obj))
         (total-height (reduce #'+ row-heights)))
    (list 0 0 *page-width* total-height)))

(with-png-file ("example.png" :rgb24 *letter-width* *letter-height*)
  (new-path)
  (render (layout-background +white+))
  (translate *margin* *margin*)
  (render (layout-columns
            3
            (list (layout-text "col1a")
                  (layout-text "col2a")
                  (layout-text "col3a")
                  (layout-text "col1b")
                  (layout-text "col2b")))))


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
