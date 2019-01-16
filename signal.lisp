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
  (let ((x (mod x (* 2 PI))))
    (cond ((>= x 3PI/2) (- (* PI/2 x) 1.5))
          ((and (<= PI/2 x) (< x 3PI/2)) (+ (* -1 PI/2 x) 1.0))
          (t (* PI/2 x)))))

(defun saw (x)
  (let ((x (mod x (* 2 PI))))
    (cond ((>= x PI) (- (/ x PI) 1)
          ((t (/ x PI)))))))
