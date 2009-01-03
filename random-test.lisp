(in-package :learnr)

(def-suite random)
(in-suite random)

(test real-random "test to make sure range on real random is working"
  (let ((rng (make-prng :state (make-random-state t))))
    (dotimes (rep 100)
             (let ((x (get-random rng 10 20)))
               (is (and (<= 10 x) (>= 20 x)))))
    (dotimes (rep 100)
             (let ((x (get-random rng -20 20)))
               (is (and (<= -20 x) (>= 20 x)))))))

(test mock-random "mock random works"
  (let ((rng (make-mock-rng :state (list 15 18 20))))
    (is (= 15 (get-random rng 10 20)))
    (is (= 18 (get-random rng 10 20)))
    (is (= 20 (get-random rng 10 20)))))

(run! 'random)

