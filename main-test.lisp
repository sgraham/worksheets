(in-package :learnr)

(def-suite main)
(in-suite main)

(test document-setup
  (let ((doc (make-instance 'document)))
       doc))

(run 'main)
