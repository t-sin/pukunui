(defpackage #:pukunui/timeinfo
  (:use #:cl)
  (:export #:timeinfo
           #:make-timeinfo
           #:timeinfo-p
           #:timeinfo-tick
           #:timeinfo-bpm

           #:make-timeinfo*
           #:update-timeinfo))
(in-package #:pukunui/timeinfo)

(defstruct timeinfo
  tick bpm)

(defun make-timeinfo* ()
  (make-timeinfo :tick 0 :bpm 110))

(defun update-timeinfo (timeinfo)
  (incf (timeinfo-tick timeinfo)))
