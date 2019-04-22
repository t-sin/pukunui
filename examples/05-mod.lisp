;; (ql:quickload :pukunui)

(use-package '(:pukunui/pcm
               :pukunui/playinfo
               :pukunui/signal
               :pukunui/units/core
               :pukunui/units/oscillator
               :pukunui/units/effect
               :pukunui/units/sample
               :pukunui/event))

;; sine wave simply
(let ((sine (create-sine 440)))
  (setf (unit-gain sine) 0.3)
  (pukunui:init sine))

;; sine wave with amplifer modulation (AM)
(let ((sine (create-sine 440)))
  (setf (unit-gain sine) (create-sine 5))
  (pukunui:init sine))

;; sine wave with frequency modulation (FM)
(let* ((offset (create-uoffset 440))
       (amp (create-uamp 20))
       (sine (create-sine offset)))
  (setf (unit-src amp) (create-sine 10))
  (setf (unit-src offset) amp)
  (setf (unit-gain sine) 0.3)
  (pukunui:init sine))

;; TODO: レトロなアレ
