(defpackage #:pukunui-test/tests/signal
  (:use #:cl
        #:rove)
  (:import-from #:pukunui/signal
                #:pan))
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
