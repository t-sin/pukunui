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
(let ((sine (create-sine)))
  (setf (osc-freq sine) 440)
  (setf (unit-gain sine) 0.3)
  (pukunui:init sine))

;; sine wave with amplifer modulation (AM)
(let ((sine (create-sine))
      (mod (create-sine)))
  (setf (osc-freq sine) 440)
  (setf (osc-freq mod) 5)
  (setf (unit-gain sine) mod)
  (pukunui:init sine))

;; sample loop with AM
(progn
  (setf (sample-idx *ev*) 0)
  (setf (sample-loop-p *ev*) t)
  (setf (sample-playing-p *ev*) t)
  (setf (unit-gain *ev*) (create-sine 5))
  (pukunui:init *ev*))

;; rotary speaker
(let ((mod (create-sine)))
  (setf (osc-freq mod) 5)
  (setf (sample-idx *ev*) 0)
  (setf (sample-loop-p *ev*) t)
  (setf (sample-playing-p *ev*) t)
  (setf (unit-pan *ev*) mod)
  (pukunui:init *ev*))

;; sine wave with frequency modulation (FM)
(let ((offset (create-uoffset 440))
      (amp (create-uamp 20))
      (mod (create-sine))
      (sine (create-sine)))
  (setf (osc-freq mod) 10)
  (setf (osc-freq sine) offset)
  (setf (unit-src amp) mod)
  (setf (unit-src offset) amp)
  (setf (unit-gain sine) 0.3)
  (pukunui:init sine))
