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
(defparameter *dr* (read-wav (asdf:system-relative-pathname :pukunui "resouces/dr.wav")))

;; play a WAV file Oneshot
(progn
  (setf (sample-idx *ev*) 0)
  (setf (sample-loop-p *ev*) nil)
  (setf (sample-playing-p *ev*) t)
  (setf (unit-gain *ev*) 0.8)
  (pukunui:init *ev*))

;; play a WAV file ad infinitum
(progn
  (setf (sample-idx *ev*) 0)
  (setf (sample-loop-p *ev*) t)
  (setf (sample-playing-p *ev*) t)
  (setf (unit-gain *ev*) 0.8)
  (pukunui:init *ev*))

;; play two samples
(let ((mixer (create-umix)))
  (setf (sample-idx *ev*) 0)
  (setf (sample-loop-p *ev*) t)
  (setf (sample-playing-p *ev*) t)
  (setf (unit-gain *ev*) 0.8)

  (setf (sample-idx *dr*) 0)
  (setf (sample-loop-p *dr*) t)
  (setf (sample-playing-p *dr*) t)
  (setf (unit-gain *dr*) 0.8)

  (setf (unit-src mixer) (vector *ev* *dr*))
  (pukunui:init mixer))
