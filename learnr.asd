(defpackage #:learnr-asd
  (:use :cl :asdf))

(in-package #:learnr-asd)

(defsystem learnr
  :description "learnr.org/worksheets"
  :components ((:file "packages")
               (:file "random" :depends-on ("packages"))
               (:file "random-test" :depends-on ("packages" "random"))
               (:file "questions" :depends-on ("packages" "properties"))
               (:file "questions-test" :depends-on ("packages" "questions"))
               (:file "properties" :depends-on ("packages"))
               (:file "properties-test" :depends-on ("packages" "properties"))
               (:file "main" :depends-on ("packages"))
               (:file "main-test" :depends-on ("main")))
  :depends-on (:cl-cairo2 :FiveAM))
