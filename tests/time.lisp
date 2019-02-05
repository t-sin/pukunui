(defpackage #:pukunui-test/tests/time
  (:use #:cl
        #:rove)
  (:import-from #:pukunui/playinfo
                #:make-time
                #:time=
                #:time<))
(in-package #:pukunui-test/tests/time)

(defun test-time= (tlis1 tlis2 expected)
  (destructuring-bind (bar1 beat1 pos1)
      tlis1
    (destructuring-bind (bar2 beat2 pos2)
        tlis2
      (ok (eql (time= (make-time :bar bar1 :beat beat1 :pos pos1)
                      (make-time :bar bar2 :beat beat2 :pos pos2))
               expected)))))

(deftest compare-time-equality
  (testing "equal"
    (test-time= '(0 0 0) '(0 0 0) t)
    (test-time= '(1 0 0) '(1 0 0) t))

  (testing "less"
    (test-time= '(0 0 0) '(1 0 0) nil)
    (test-time= '(0 0 0) '(0 1 0) nil)
    (test-time= '(0 0 0) '(0 0 1) nil))

  (testing "greater"
    (test-time= '(1 0 0) '(0 0 0) nil)
    (test-time= '(0 1 0) '(0 0 0) nil)
    (test-time= '(0 0 1) '(0 0 0) nil)))

(defun test-time< (tlis1 tlis2 expected)
  (destructuring-bind (bar1 beat1 pos1)
      tlis1
    (destructuring-bind (bar2 beat2 pos2)
        tlis2
      (ok (eql (time< (make-time :bar bar1 :beat beat1 :pos pos1)
                      (make-time :bar bar2 :beat beat2 :pos pos2))
               expected)))))

(deftest compare-greater-time
  (testing "equal"
    (test-time< '(0 0 0) '(0 0 0) nil)
    (test-time< '(1 0 0) '(1 0 0) nil))

  (testing "less"
    (test-time< '(0 0 0) '(1 0 0) t)
    (test-time< '(0 0 0) '(0 1 0) t)
    (test-time< '(0 0 0) '(0 0 1) t))

  (testing "greater"
    (test-time< '(1 0 0) '(0 0 0) nil)
    (test-time< '(0 1 0) '(0 0 0) nil)
    (test-time< '(0 0 1) '(0 0 0) nil)))
