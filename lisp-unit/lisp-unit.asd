(defpackage #:lisp-unit-asd
  (:use :cl :asdf))

(in-package :lisp-unit-asd)

(defsystem lisp-unit
  :name "lisp-unit"
  :author "Chris Riesbeck"
  :description "A test suite package, modelled after JUnit."
  :components ((:file "lisp-unit")))
