;; (ql:quickload :pukunui)

(use-package '(:pukunui/pcm
               :pukunui/playinfo
               :pukunui/signal
               :pukunui/units/core
               :pukunui/units/oscillator
               :pukunui/units/effect
               :pukunui/units/sample
               :pukunui/event))

(defparameter *ev* (read-wav (asdf:system-relative-pathname :pukunui "resouces/ev.wav")))

;; sine wave simply
(let ((sine (create-sine 440)))
  (setf (unit-gain sine) 0.3)
  (pukunui:init sine))

;; sine wave with amplifer modulation (AM)
(let ((sine (create-sine 440)))
  (setf (unit-gain sine) (create-sine 5))
  (pukunui:init sine))

;; sample loop with AM
(progn
  (setf (sample-idx *ev*) 0)
  (setf (sample-loop-p *ev*) t)
  (setf (sample-playing-p *ev*) t)
  (setf (unit-gain *ev*) (create-sine 5))
  (pukunui:init *ev*))

;; rotary speaker
(progn
  (setf (sample-idx *ev*) 0)
  (setf (sample-loop-p *ev*) t)
  (setf (sample-playing-p *ev*) t)
  (setf (unit-pan *ev*) (create-sine 5))
  (pukunui:init *ev*))

;; sine wave with frequency modulation (FM)
(let* ((offset (create-uoffset 440))
       (amp (create-uamp 20))
       (sine (create-sine offset)))
  (setf (unit-src amp) (create-sine 10))
  (setf (unit-src offset) amp)
  (setf (unit-gain sine) 0.3)
  (pukunui:init sine))

;; TODO: レトロなアレ
(let* ((sine1 (create-sine 440))
       (sine2 (create-sine 440)))
  (setf (osc-init-ph sine1) (create-sine 80))
  (setf (unit-gain sine1) 0.3)
  (pukunui:init sine1))
