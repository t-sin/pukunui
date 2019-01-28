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

(let ((clip (read-wav (asdf:system-relative-pathname :pukunui "ev.wav")))
      (gain-mod (create-offset 0.7)))
  (setf (unit-src gain-mod) (create-sine 39))
  (setf (clip-playing-p clip) t)
  (setf (clip-loop-p clip) t)
  (setf *unit-graph* (create-unit clip 0.8)))

(defun start ()
  (let ((th (bt:make-thread (pastart (make-paconf*) #'calc-toplevel)
                            :name "pukunui-sound-thread")))
    (setf *sound-thread* th)))

(defun stop ()
  (bt:destroy-thread *sound-thread*))
