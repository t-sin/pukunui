(defpackage #:pukunui-test/tests/signal
  (:use #:cl
        #:rove)
  (:import-from #:pukunui/signal
                #:pan))
(in-package #:pukunui-test/tests/signal)

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

  (testing "left")
  (testing "right"))
