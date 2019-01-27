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
  (let ((clip (read-wav (asdf:system-relative-pathname :pukunui "ev.wav"))))
    (setf (clip-playing-p clip) t)
    (setf (clip-loop-p clip) t)
    (setf *unit-graph* (create-unit clip 0.8)))
  (start (make-paconf*) #'calc-toplevel))
