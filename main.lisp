(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/unit
        #:pukunui/units/core)
  (:export #:start*))
(in-package #:pukunui)

(defparameter *unit-graph* nil)

(defun calc-toplevel ()
  (calc-unit *unit-graph*))

(defun start* ()
  (setf *unit-graph* (create-unit (create-osc 0) 1))
  (start (make-paconf*) #'calc-toplevel))
