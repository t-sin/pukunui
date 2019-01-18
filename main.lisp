(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/unit)
  (:export #:generate-toplevel))
(in-package #:pukunui)

(defparameter *unit-graph* nil)

(defun generate-toplevel ()
  (values 0 0))
