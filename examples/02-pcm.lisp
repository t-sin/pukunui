;; (ql:quickload :pukunui)

(use-package '(:pukunui/pcm
               :pukunui/playinfo
               :pukunui/signal
               :pukunui/units/core
               :pukunui/units/oscillator
               :pukunui/units/effect
               :pukunui/units/sample
               :pukunui/event))

;; play a WAV file Oneshot
(let ((ev (read-wav (asdf:system-relative-pathname :pukunui "resouces/ev.wav"))))
  (setf (sample-loop-p ev) nil)
  (setf (sample-playing-p ev) t)
  (setf (unit-gain ev) 0.8)
  (pukunui:init ev))

;; play a WAV file ad infinitum
(let ((ev (read-wav (asdf:system-relative-pathname :pukunui "resouces/ev.wav"))))
  (setf (sample-loop-p ev) t)
  (setf (sample-playing-p ev) t)
  (setf (unit-gain ev) 0.8)
  (pukunui:init ev))
