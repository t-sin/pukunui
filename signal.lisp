(defpackage #:pukunui/signal
  (:use #:cl)
  (:export #:pan))
(in-package #:pukunui/signal)

(defun pan (l r pan)
  (assert (and (<= -1 pan) (<= pan 1)))
  (cond ((zerop pan) (values l r))
        ((plusp pan) (values (* (- 1 pan) l) r))
        ((minusp pan) (values l (* (+ 1 pan) r)))))
