(defpackage :learnr
  (:use :common-lisp
        :cl-cairo2
        :cl-colors
        :it.bese.FiveAM))

(in-package :learnr)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
