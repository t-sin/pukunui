(defpackage #:pukunui/util
  (:use #:cl)
  (:export #:to-float))
(in-package #:pukunui/util)

;; it should be a macro?
(defun to-float (l r)
  (values (coerce l 'single-float)
          (coerce r 'single-float)))
