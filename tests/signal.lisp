(defpackage #:pukunui-test/tests/signal
  (:use #:cl
        #:rove)
  (:import-from #:pukunui/signal
                #:pan

                #:2PI
                #:PI/2
                #:3PI/2

                #:tri
                #:saw))
(in-package #:pukunui-test/tests/signal)

;;;;
;; panning

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
  (testing "values in a period"
    (test-tri 0 0)
    (test-tri (/ PI 4) 0.5)

    (test-tri PI/2 1)
    (test-tri (* (/ 3 4) PI) 0.5)

    (test-tri PI 0)
    (test-tri 3PI/2 -1)
    (test-tri 2PI 0)))

(defun test-saw (x expected)
  (ok (= (saw x) expected)))

(deftest sawtooth-function
  (testing "values in a period"
    (test-saw 0 0)
    (test-saw PI/2 0.5)
    (test-saw PI -1)
    (test-saw 3PI/2 -0.5)
    (test-saw 2PI 0)))
