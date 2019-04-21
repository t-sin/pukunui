;; (ql:quickload :pukunui)

(use-package '(:pukunui/pcm
               :pukunui/playinfo
               :pukunui/signal
               :pukunui/units/core
               :pukunui/units/oscillator
               :pukunui/units/effect
               :pukunui/units/sample
               :pukunui/event))

;; play a sine wave with 440Hz
(let ((sine (create-sine 440)))
  (setf (unit-gain sine) 0.3)
  (pukunui:init sine))

;; play a triangle wave with 440Hz
(let ((tri (create-tri 440)))
  (setf (unit-gain tri) 0.3)
  (pukunui:init tri))

;; play a saw wave with 440Hz
(let ((saw (create-saw 440)))
  (setf (unit-gain saw) 0.3)
  (pukunui:init saw))

;; play a pulse (square) wave with 440Hz with duty ratio 0.5
(let ((pulse (create-pulse 440 0.5)))
  (setf (unit-gain pulse) 0.3)
  (pukunui:init pulse))

;; play a pulse (square) wave with 440Hz with duty ratio 0.25
(let ((pulse (create-pulse 440 0.25)))
  (setf (unit-gain pulse) 0.3)
  (pukunui:init pulse))
