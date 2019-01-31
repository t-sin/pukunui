(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/timeinfo
        #:pukunui/unit

        #:pukunui/units/core
        #:pukunui/units/clip
        #:pukunui/units/oscillator

        #:pukunui/pcm)
  (:export #:init
           #:start
           #:stop))
(in-package #:pukunui)

(defparameter *unit-graph* nil)
(defparameter *paconf* nil)
(defparameter *timeinfo* nil)

(defun calc-toplevel ()
  (multiple-value-bind (l r)
      (calc-unit *unit-graph* *timeinfo*)
    (update-timeinfo *timeinfo*)
    (values l r)))

(defparameter *sound-thread* nil)

(defparameter *ev* (read-wav (asdf:system-relative-pathname :pukunui "ev.wav")))
(defparameter *dr* (read-wav (asdf:system-relative-pathname :pukunui "dr.wav")))

(let ((umix (create-umix))
      (sine (create-sine 880)))
  (setf (unit-gain sine) 0.3)
  (setf (unit-src umix) (vector *ev* *dr*))
  (setf (unit-pan umix) (create-sine 10))
  (setf (clip-playing-p *ev*) t)
  (setf (clip-loop-p *ev*) t)
  (setf (clip-playing-p *dr*) t)
  (setf (clip-loop-p *dr*) t)
  (setf *unit-graph* (create-unit sine (create-uadsr 44100 1 4000 0.2 4000) 0)))

(defun init ()
  (setf *paconf* (make-paconf*))
  (setf *timeinfo* (make-timeinfo*)))

(defun start ()
  (let ((th (bt:make-thread (pastart *paconf* #'calc-toplevel)
                            :name "pukunui-sound-thread")))
    (setf *sound-thread* th)))

(defun stop ()
  (bt:destroy-thread *sound-thread*)
  (setf *sound-thread* nil))
