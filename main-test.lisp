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
                                   (layout-text "weewaa" +red+)))))
    (assert-equal '(0.0d0 5.0d0 220.0d0 5.0d0) (extents (layout-horiz (list
                                                                        (layout-line 0 5 100 5)
                                                                        (layout-line 20 5 120 5)))))))

(define-test into-n-chunks
  (assert-equal '((a)) (into-n-sized-chunks '(a) 3))
  (assert-equal '((a b)) (into-n-sized-chunks '(a b) 3))
  (assert-equal '((a b c)) (into-n-sized-chunks '(a b c) 3))
  (assert-equal '((a b c) (d)) (into-n-sized-chunks '(a b c d) 3))
  (assert-equal '((a b c) (d e f)) (into-n-sized-chunks '(a b c d e f) 3))
  (assert-equal '((a b c) (d e f) (g)) (into-n-sized-chunks '(a b c d e f g) 3))
  (assert-equal '((a b) (c d) (e f) (g)) (into-n-sized-chunks '(a b c d e f g) 2))
  (assert-equal '((a) (b) (c) (d) (e)) (into-n-sized-chunks '(a b c d e) 1)))

(define-test column-extents
  (with-null-pdf-context
    (assert-equal (list 0 0 *page-width* 18.0d0)
                  (extents (layout-columns 2 (list
                                               (layout-line 0 5 100 20)
                                               (layout-line 20 10 120 23)))))
    (assert-equal (list 0 0 *page-width* 28.0d0)
                  (extents (layout-columns 1 (list
                                               (layout-line 0 5 100 20)
                                               (layout-line 20 10 120 23)))))))

(define-test get-col-offsets
  (assert-equal '(0 50 100) (get-col-offsets 3 150))
  (assert-equal '(0 1/2) (get-col-offsets 2 1))
  (assert-equal '(0) (get-col-offsets 1 50)))

(define-test row-offsets-from-heights
  (assert-equal '(0 15 33) (row-offsets-from-heights '(15 18 10))))


(run-tests)
