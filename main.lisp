(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/unit

        #:pukunui/units/core
        #:pukunui/units/clip
        #:pukunui/units/oscillator

        #:pukunui/pcm)
  (:export #:start
           #:stop))
(in-package #:pukunui)

(defparameter *unit-graph* nil)

(defun calc-toplevel ()
  (calc-unit *unit-graph*))

(defparameter *sound-thread* nil)

(defparameter *ev* (read-wav (asdf:system-relative-pathname :pukunui "ev.wav")))
(defparameter *dr* (read-wav (asdf:system-relative-pathname :pukunui "dr.wav")))

(let ((ugroup (create-ugroup)))
  (setf (unit-src ugroup) (vector *ev* *dr*))
  (setf (clip-playing-p *ev*) t)
  (setf (clip-loop-p *ev*) t)
  (setf (clip-playing-p *dr*) t)
  (setf (clip-loop-p *dr*) t)
  (setf *unit-graph* (create-unit ugroup 0.3)))

(defun start ()
  (let ((th (bt:make-thread (pastart (make-paconf*) #'calc-toplevel)
                            :name "pukunui-sound-thread")))
    (setf *sound-thread* th)))

(defun stop ()
  (bt:destroy-thread *sound-thread*))
