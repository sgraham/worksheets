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
    (assert-equal '(0.0d0 5.0d0 100.0d0 5.0d0) (extents (layout-line 0 5 100 5)))
    (assert-equal '(5.0d0 5.0d0 100.0d0 88.0d0) (extents (layout-line 5 5 100 88)))))

(define-test text-extents
  (with-null-pdf-context
    (assert-equal '(0.0d0 0.0d0 0.0d0 0.0d0) (extents (layout-text "")))
    (assert-equal '(0.15625d0 -6.65625d0 49.66796875d0 0.15625d0) (extents (layout-text "weeeeee")))))

(define-test hcentre-extents
  (with-null-pdf-context
    (assert-equal '(-2.0d0 -5.0d0 106.0d0 107.0d0) 
                  (extents
                    (layout-hcentre (list
                                      (layout-line 0 0 100 100)
                                      (layout-line 50 50 100 100)
                                      (layout-line -2 -5 106 107)))))
    (assert-equal '(0.0d0 0.0d0 200.0d0 200.0d0)
                  (extents
                    (layout-hcentre (list
                                      (layout-line 0 0 50 50)
                                      (layout-line 100 100 200 200)))))))

(define-test horiz-extents
  (with-null-pdf-context
    (assert-equal '(0.0d0 0.0d0 80.0d0 0.0d0)
                  (extents (layout-horiz (list
                                           (layout-line 0 0 50 0)
                                           (layout-line 0 0 30 0)))))
    (assert-equal '(0.0d0 -6.65625d0 72.0d0 5.0d0)
                  (extents (layout-hcentre
                             (list (layout-line 0 5 72 5)
                                   (layout-text "weewaa" +red+)))))))


(run-tests)
