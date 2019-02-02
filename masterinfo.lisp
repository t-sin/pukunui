(defpackage #:pukunui/masterinfo
  (:use #:cl
        #:pukunui/constants)
  (:export #:masterinfo
           #:make-masterinfo
           #:masterinfo-p
           #:masterinfo-tick
           #:masterinfo-bpm
           #:masterinfo-frames-per-buffer
           #:masterinfo-sample-rate

           #:update-masterinfo))
(in-package #:pukunui/masterinfo)

(defstruct masterinfo
  (frames-per-buffer +frames-per-buffer+)
  (sample-rate +sample-rate+)
  (bpm +bpm+)
  (tick 0))

(defun update-masterinfo (masterinfo)
  (incf (masterinfo-tick masterinfo)))
