;; (ql:quickload :pukunui)

(use-package '(:pukunui/pcm
               :pukunui/playinfo
               :pukunui/signal
               :pukunui/units/core
               :pukunui/units/oscillator
               :pukunui/units/effect
               :pukunui/units/sample
               :pukunui/event))

(defparameter *dr*
  (read-wav (asdf:system-relative-pathname :pukunui "resouces/dr.wav")))

;; play a drum sample without delay
(progn
  (setf (sample-idx *dr*) 0)
  (setf (sample-loop-p *dr*) t)
  (setf (sample-playing-p *dr*) t)
  (setf (unit-gain *dr*) 0.8)
  (pukunui:init *dr*))

;; play a drum sample with delay
(let ((delay (create-delay-1* 0.5 0.2)))
  (setf (sample-idx *dr*) 0)
  (setf (sample-loop-p *dr*) t)
  (setf (sample-playing-p *dr*) t)
  (setf (unit-gain *dr*) 0.8)

  (setf (unit-src delay) *dr*)
  (pukunui:init delay))
