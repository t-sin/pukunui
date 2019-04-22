;; (ql:quickload :pukunui)

(use-package '(:pukunui/pcm
               :pukunui/playinfo
               :pukunui/signal
               :pukunui/units/core
               :pukunui/units/oscillator
               :pukunui/units/effect
               :pukunui/units/sample
               :pukunui/event))

(defparameter *ev*
  (read-wav (asdf:system-relative-pathname :pukunui "resouces/ev.wav")))
(defparameter *dr*
  (pukunui/pcm:read-wav (asdf:system-relative-pathname :pukunui "resouces/dr.wav")))

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

(let ((umix (create-umix))
      (sine (create-sine 880))
      (seq (create-useq* *space-invador-seq* 10 10 1 0))
      (delay (create-delay-1* 0.5 0.1)))

  (setf (unit-gain sine) 0.3)
  (setf (unit-src umix) (vector *ev* *dr*))
  (setf (unit-pan umix) (create-sine 10))
  (setf (sample-playing-p *ev*) t)
  (setf (sample-loop-p *ev*) t)
  (setf (sample-playing-p *dr*) t)
  (setf (sample-loop-p *dr*) t)

  (setf (unit-src delay) (create-unit seq 0.3 0))
  (pukunui:init (create-unit seq 0.2 0)))
