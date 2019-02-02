(defpackage #:pukunui/masterinfo
  (:use #:cl)
  (:export #:masterinfo
           #:make-masterinfo
           #:masterinfo-p
           #:masterinfo-tick
           #:masterinfo-bpm

           #:make-masterinfo*
           #:update-masterinfo))
(in-package #:pukunui/masterinfo)

(defstruct masterinfo
  tick bpm)

(defun make-masterinfo* ()
  (make-masterinfo :tick 0 :bpm 110))

(defun update-masterinfo (masterinfo)
  (incf (masterinfo-tick masterinfo)))
