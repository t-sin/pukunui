(defpackage #:pukunui-test/tests/signal
  (:use #:cl
        #:rove)
  (:import-from #:pukunui/signal
                #:gain
                #:gain-2
                #:mix
                #:mix-2
                #:pan

                #:2PI
                #:PI/2
                #:3PI/2

                #:tri
                #:saw
                #:pulse))
(in-package #:pukunui-test/tests/signal)

;;;;
;; panning

(defun test-gain (s g expected)
  (ok (= (gain s g) expected)))

(deftest calculate-gain
  (testing "value 1"
    (test-gain 1 1 1)
    (test-gain 1 0.5 0.5)
    (test-gain 1 0 0))
  (testing "value 0"
    (test-gain 0 1 0)
    (test-gain 0 0.5 0)
    (test-gain 0 0 0))
  (testing "value 0.5"
    (test-gain 0.5 1 0.5)
    (test-gain 0.5 0.5 0.25)
    (test-gain 0.5 0 0)))

(defun test-gain-2 (l r g el er)
  (multiple-value-bind (l r)
      (gain-2 l r g)
    (ok (= l el))
    (ok (= r er))))

(deftest calculate-stereo-gain
  (testing "max/min"
    (test-gain-2 1 1 1 1 1)
    (test-gain-2 1 1 0 0 0))
  (testing "left/right"
    (test-gain-2 1 0 0.5 0.5 0)
    (test-gain-2 0 1 0.5 0 0.5)))

(defun test-mix (s1 s2 expected)
  (ok (= (mix s1 s2) expected)))

(deftest mixing
  (test-mix 1 1 2)
  (test-mix 0 1 1)
  (test-mix 1 0 1)
  (test-mix 0 0 0))

(defun test-mix-2 (l1 r1 l2 r2  el er)
  (multiple-value-bind (l r)
      (mix-2 l1 r1 l2 r2)
    (ok (= l el))
    (ok (= r er))))

(deftest stereo-mixing
  (test-mix-2 0 1 1 0 1 1)
  (test-mix-2 1 0 0 1 1 1))

(defun test-pan (l r p el er)
  (multiple-value-bind (al ar)
      (pan l r p)
    (ok (= al el))
    (ok (= ar er))))

(deftest calculate-panning
  (testing "neutral"
    (let ((pan 0))
      (testing "zero"
        (test-pan 0 0 pan 0 0))
      (testing "max"
        (test-pan 1 1 pan 1 1)
       (test-pan -1 -1 pan -1 -1))
      (testing "max pan"
        (test-pan 1 0 pan 1 0)
        (test-pan 0 1 pan 0 1))
      (testing "other values"
        (test-pan 0.5 0.5 pan 0.5 0.5)
        (test-pan 0.7 0.3 pan 0.7 0.3)
        (test-pan 0.3 0.7 pan 0.3 0.7))))

  (testing "left"
    (let ((pan -1))
      (testing "zero"
        (test-pan 0 0 pan 0 0))
      (testing "max"
        (test-pan 1 1 pan 1 0)
        (test-pan -1 -1 pan -1 0))
      (testing "max pan"
        (test-pan 1 0 pan 1 0)
        (test-pan 0 1 pan 0 0))
      (testing "other values"
        (test-pan 0.5 0.5 pan 0.5 0)
        (test-pan 0.7 0.3 pan 0.7 0)
        (test-pan 0.3 0.7 pan 0.3 0))))

  (testing "right"
    (let ((pan 1))
      (testing "zero"
        (test-pan 0 0 pan 0 0))
      (testing "max"
        (test-pan 1 1 pan 0 1)
        (test-pan -1 -1 pan 0 -1))
      (testing "max pan"
        (test-pan 1 0 pan 0 0)
        (test-pan 0 1 pan 0 1))
      (testing "other values"
        (test-pan 0.5 0.5 pan 0 0.5)
        (test-pan 0.7 0.3 pan 0 0.3)
        (test-pan 0.3 0.7 pan 0 0.7))))

  (testing "other"
    (let ((pan 0.5))
      (testing "zero"
        (test-pan 0 0 pan 0 0))
      (testing "max"
        (test-pan 1 1 pan 0.5 1)
        (test-pan -1 -1 pan -0.5 -1))
      (testing "max pan"
        (test-pan 1 0 pan 0.5 0)
        (test-pan 0 1 pan 0 1))
      (testing "other values"
        (test-pan 0.5 0.5 pan 0.25 0.5)
        (test-pan 0.7 0.3 pan 0.35 0.3)
        (test-pan 0.3 0.7 pan 0.15 0.7)))

    (let ((pan -0.5))
      (testing "zero"
        (test-pan 0 0 pan 0 0))
      (testing "max"
        (test-pan 1 1 pan 1 0.5)
        (test-pan -1 -1 pan -1 -0.5))
      (testing "max pan"
        (test-pan 1 0 pan 1 0)
        (test-pan 0 1 pan 0 0.5))
      (testing "other values"
        (test-pan 0.5 0.5 pan 0.5 0.25)
        (test-pan 0.7 0.3 pan 0.7 0.15)
        (test-pan 0.3 0.7 pan 0.3 0.35)))))

(defun test-tri (x expected)
  (ok (= (tri x) expected)))

(deftest triangle-function
  (testing "peek values in a period"
    (test-tri 0 0)
    (test-tri (/ 1 4) 1)
    (test-tri (/ 1 2) 0)
    (test-tri (/ 3 4) -1)
    (test-tri 1 0))

  (testing "values in a period"
    (test-tri (/ 1 8) 0.5)
    (test-tri (/ 3 8) 0.5)
    (test-tri (/ 5 8) -0.5)
    (test-tri (/ 7 8) -0.5))

  (testing "values out of period"
    (testing "minus side of range"
      (test-tri (- (/ 1 4)) -1)
      (test-tri (- (/ 1 2)) 0))
    (testing "plus side of range"
      (test-tri (1+ (/ 1 4)) 1)
      (test-tri (1+ (/ 1 2)) 0))))

(defun test-saw (x expected)
  (ok (= (saw x) expected)))

(deftest sawtooth-function
  (testing "peek values in a period"
    (test-saw 0 0)
    (test-saw (/ 1 4) 0.5)
    (test-saw (/ 1 2) -1)
    (test-saw (/ 3 4) -0.5)
    (test-saw 1 0))

  (testing "values in a period"
    (test-saw (/ 1 8) 0.25)
    (test-saw (/ 3 8) 0.75)
    (test-saw (/ 5 8) -0.75)
    (test-saw (/ 7 8) -0.25))

  (testing "values out of period"
    (testing "minus side of range"
      (test-saw (- (/ 1 4)) -0.5)
      (test-saw (- (/ 1 2)) -1))
    (testing "plus side of range"
      (test-saw (1+ (/ 1 4)) 0.5)
      (test-saw (1+ (/ 1 2)) -1))))

(defun test-pulse (x expected &optional (duty 0.5))
  (ok (= (pulse x duty) expected)))

(deftest pulse-function
  (testing "values in period (duty: 0.5)"
    (test-pulse 0 1)
    (test-pulse (/ 1 4) 1)
    (test-pulse (/ 1 2) -1)
    (test-pulse (/ 3 4) -1)
    (test-pulse 1 1))

  (testing "values in period (duty: 0.5)"
    (test-pulse (/ 1 8) 1)
    (test-pulse (/ 3 8) 1)
    (test-pulse (/ 5 8) -1)
    (test-pulse (/ 7 8) -1))

  (testing "values out of period"
    (testing "minus side of range"
      (test-pulse (- (/ 1 4)) -1)
      (test-pulse (- (/ 1 2)) -1))
    (testing "plus side of range"
      (test-pulse (1+ (/ 1 4)) 1)
      (test-pulse (1+ (/ 1 2)) -1)))

  (testing "duty ratio"
    (let ((duty 0.25))
      (test-pulse 0 1 duty)
      (test-pulse (/ 1 4) -1 duty)
      (test-pulse (/ 2 4) -1 duty))
    (let ((duty 0.75))
      (test-pulse 0 1 duty)
      (test-pulse (/ 0 4) 1 duty)
      (test-pulse (/ 1 4) 1 duty)
      (test-pulse (/ 2 4) 1 duty)
      (test-pulse (/ 3 4) -1 duty)
      (test-pulse (/ 4 4) 1 duty))))
