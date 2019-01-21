(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/unit)
  (:export #:generate-toplevel))
(in-package #:pukunui)

(defparameter *unit-graph* nil)

(defun calc-toplevel ()
  (calc-unit *unit-graph*))

(defun start* ()
  (setf *unit-graph* (create-unit (create-sin 0) 1))
  (start (make-paconf*) #'calc-toplevel))
