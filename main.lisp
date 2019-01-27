(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/unit

        #:pukunui/units/core
        #:pukunui/units/clip
        #:pukunui/units/oscillator

        #:pukunui/pcm)
  (:export #:start*))
(in-package #:pukunui)

(defparameter *unit-graph* nil)

(defun calc-toplevel ()
  (calc-unit *unit-graph*))

(defun start* ()
  (let ((clip (read-wav (asdf:system-relative-pathname :pukunui "ev.wav")))
        (gain-mod (create-offset 0.7)))
    (setf (unit-src gain-mod) (create-sine 39))
    (setf (clip-playing-p clip) t)
    (setf (clip-loop-p clip) t)
    (setf *unit-graph* (create-unit clip gain-mod)))
  (start (make-paconf*) #'calc-toplevel))
