;; (ql:quickload :pukunui)

(use-package '(:pukunui/pcm
               :pukunui/playinfo
               :pukunui/signal
               :pukunui/units/core
               :pukunui/units/oscillator
               :pukunui/units/effect
               :pukunui/units/sample
               :pukunui/event))

;; play a drum sample without delay
(let ((dr (read-wav (asdf:system-relative-pathname :pukunui "resouces/dr.wav"))))
  (setf (sample-loop-p dr) t)
  (setf (sample-playing-p dr) t)
  (setf (unit-gain dr) 0.8)
  (pukunui:init dr))

;; play a drum sample with delay
(let ((dr (read-wav (asdf:system-relative-pathname :pukunui "resouces/dr.wav")))
      (delay (create-delay-1* 0.5 0.2)))
  (setf (sample-loop-p dr) t)
  (setf (sample-playing-p dr) t)
  (setf (unit-gain dr) 0.8)

  (setf (unit-src delay) dr)
  (pukunui:init delay))
