(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/playinfo
        #:pukunui/unit

        #:pukunui/units/core
        #:pukunui/units/sample
        #:pukunui/units/oscillator
        #:pukunui/units/effect

        #:pukunui/event

        #:pukunui/pcm)
  (:export #:init
           #:start
           #:stop))
(in-package #:pukunui)

(defparameter *unit-graph* nil)
(defparameter *paconf* nil)
(defparameter *masterinfo* nil)

(defun calc-toplevel ()
  (multiple-value-bind (l r)
      (calc-unit *unit-graph* *masterinfo*)
    (update-masterinfo *masterinfo*)
    (values l r)))

(defparameter *sound-thread* nil)

(defparameter *ev* (read-wav (asdf:system-relative-pathname :pukunui "resouces/ev.wav")))
(defparameter *dr* (read-wav (asdf:system-relative-pathname :pukunui "resouces/dr.wav")))

(defparameter *space-invador-seq*
  `((,(make-timepos :bar 0 :beat 0 :pos 0) :on ,(note->freq 48))
    (,(make-timepos :bar 0 :beat 0 :pos 0.25) :off)
    (,(make-timepos :bar 0 :beat 1 :pos 0) :on ,(note->freq 46))
    (,(make-timepos :bar 0 :beat 1 :pos 0.25) :off)
    (,(make-timepos :bar 0 :beat 2 :pos 0) :on ,(note->freq 44))
    (,(make-timepos :bar 0 :beat 2 :pos 0.25) :off)
    (,(make-timepos :bar 0 :beat 3 :pos 0) :on ,(note->freq 44))
    (,(make-timepos :bar 0 :beat 3 :pos 0.25) :off)
    (,(make-timepos :bar 1 :beat 0 :pos 0) :on ,(note->freq 48))
    (,(make-timepos :bar 1 :beat 0 :pos 0.25) :off)
    (,(make-timepos :bar 1 :beat 1 :pos 0) :on ,(note->freq 46))
    (,(make-timepos :bar 1 :beat 1 :pos 0.25) :off)
    (,(make-timepos :bar 1 :beat 2 :pos 0) :on ,(note->freq 44))
    (,(make-timepos :bar 1 :beat 2 :pos 0.25) :off)
    (,(make-timepos :bar 1 :beat 3 :pos 0) :on ,(note->freq 44))
    (,(make-timepos :bar 1 :beat 3 :pos 0.25) :off)))

(defun init-ugraph ()
  (let ((umix (create-umix))
        (sine (create-sine 880))
        (seq (create-useq* *space-invador-seq*))
        (delay (create-delay-1* 0.5 0.1)))
        ;; (delay (create-delay* 44100)))
    (setf (unit-gain sine) 0.3)
    (setf (unit-src umix) (vector *ev* *dr*))
    (setf (unit-pan umix) (create-sine 10))
    (setf (sample-playing-p *ev*) t)
    (setf (sample-loop-p *ev*) t)
    (setf (sample-playing-p *dr*) t)
    (setf (sample-loop-p *dr*) t)

    (setf (unit-src delay) (create-unit seq 0.5 0))
    (setf *unit-graph* delay)))

(defun init ()
  (setf *masterinfo* (make-masterinfo))
  (setf (masterinfo-bpm *masterinfo*) 80)
  (init-ugraph))

(defun start ()
  (unless *sound-thread*
    (let ((th (bt:make-thread (pastart *masterinfo* #'calc-toplevel)
                              :name "pukunui-sound-thread")))
      (setf *sound-thread* th))))

(defun stop ()
  (bt:destroy-thread *sound-thread*)
  (setf *sound-thread* nil))
