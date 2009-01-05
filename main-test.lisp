(asdf:oos 'asdf:load-op :lisp-unit)

(in-package :learnr)

(use-package :lisp-unit)

(defmacro assert-in-range (val min max)
  (let ((n (gensym)))
    `(let ((,n ,val))
       (assert-true (and (<= ,min ,n) (>= ,max ,n))))))

(define-test real-random
  (let ((rng (make-prng :state (make-random-state t))))
    (dotimes (rep 100)
      (assert-in-range (get-random rng 10 20) 10 20))
    (dotimes (rep 100)
      (assert-in-range (get-random rng -20 20) -20 20))))

(define-test mock-random
  (let ((rng (make-mock-rng :state (list 15 18 20))))
    (assert-equal 15 (get-random rng 10 20))
    (assert-equal 18 (get-random rng 10 20))
    (assert-equal 20 (get-random rng 10 20))))

(define-test line-extents
  (with-null-pdf-context
    (assert-equal '(0.0d0 5.0d0 100.0d0 5.0d0) (extents (layout-line 0 5 100 5)))))

(run-tests)
