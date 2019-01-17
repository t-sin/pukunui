(defpackage #:pukunui/signal
  (:use #:cl)
  (:export #:pan

           #:2PI
           #:PI/2
           #:3PI/2

           #:tri
           #:saw))
(in-package #:pukunui/signal)

(defun pan (l r pan)
  (assert (and (<= -1 pan) (<= pan 1)))
  (cond ((zerop pan) (values l r))
        ((plusp pan) (values (* (- 1 pan) l) r))
        ((minusp pan) (values l (* (+ 1 pan) r)))))

(defconstant 2PI (* 2 PI))
(defconstant PI/2 (/ PI 2))
(defconstant 3PI/2 (* 3 (/ PI 2)))

(defun tri (x)
  (let* ((x (mod x 1))
         (y (* 4 x)))
    (cond ((>= x (/ 3 4)) (- y 4))
          ((and (<= (/ 1 4) x) (< x (/ 3 4))) (+ (- y) 2))
          (t y))))

(defun saw (x)
  (let* ((x (mod x 1))
         (y (* x 2)))
    (cond ((>= x (/ 1 2)) (- y 2))
          (t y))))
