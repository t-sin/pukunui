(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/unit

        #:pukunui/units/core
        #:pukunui/units/oscillator)
  (:export #:start*))
(in-package #:pukunui)

(defparameter *unit-graph* nil)

(defun calc-toplevel ()
  (calc-unit *unit-graph*))

(defun start* ()
  (let* ((freq-mod (create-offset 880))
         (amp-mod (create-amp 20)))
    (setf (unit-src amp-mod) (create-sine 7))
    (setf (unit-src freq-mod) amp-mod)
    (setf *unit-graph* (create-unit (create-sine freq-mod) 0.8)))
  (start (make-paconf*) #'calc-toplevel))
