(defpackage #:pukunui/signal
  (:use #:cl)
  (:export #:pan))
(in-package #:pukunui/signal)

(defun pan (l r pan)
  (values l r))
