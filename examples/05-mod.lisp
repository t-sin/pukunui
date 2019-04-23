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

(defparameter *kb-seq*
  `((,(make-timepos :bar 0 :beat 3 :pos 0) :on ,(note->freq 67))
    (,(make-timepos :bar 0 :beat 3 :pos 0.5) :off)
    (,(make-timepos :bar 0 :beat 3 :pos 0.75) :on ,(note->freq 67))
    (,(make-timepos :bar 1 :beat 0 :pos 0) :on ,(note->freq 72))
    (,(make-timepos :bar 1 :beat 2 :pos 0) :off)
    (,(make-timepos :bar 1 :beat 3 :pos 0) :on ,(note->freq 72))
    (,(make-timepos :bar 1 :beat 3 :pos 0.75) :on ,(note->freq 76))
    (,(make-timepos :bar 2 :beat 0 :pos 0) :on ,(note->freq 79))
    (,(make-timepos :bar 2 :beat 0 :pos 0.5) :on ,(note->freq 84))
    (,(make-timepos :bar 2 :beat 0 :pos 0.75) :off)
    (,(make-timepos :bar 2 :beat 1 :pos 0) :on ,(note->freq 83))
    (,(make-timepos :bar 2 :beat 1 :pos 0.25) :off)
    (,(make-timepos :bar 2 :beat 1 :pos 0.5) :on ,(note->freq 81))
    (,(make-timepos :bar 2 :beat 1 :pos 0.75) :off)
    (,(make-timepos :bar 2 :beat 2 :pos 0) :on ,(note->freq 79))
    (,(make-timepos :bar 2 :beat 3 :pos 0) :on ,(note->freq 76))
    (,(make-timepos :bar 2 :beat 3 :pos 0.75) :on ,(note->freq 79))
    (,(make-timepos :bar 3 :beat 0 :pos 0) :on ,(note->freq 77))
    (,(make-timepos :bar 3 :beat 1 :pos 0) :on ,(note->freq 74))
    (,(make-timepos :bar 3 :beat 1 :pos 0.75) :on ,(note->freq 76))
    (,(make-timepos :bar 3 :beat 2 :pos 0) :on ,(note->freq 74))
    (,(make-timepos :bar 3 :beat 3 :pos 0) :on ,(note->freq 76))
    (,(make-timepos :bar 3 :beat 3 :pos 0.75) :on ,(note->freq 74))
    (,(make-timepos :bar 4 :beat 0 :pos 0) :on ,(note->freq 72))
    (,(make-timepos :bar 4 :beat 3 :pos 0) :off)))

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

;; FM
(let* ((sine (create-sine))
       (mod (create-sine))
       (seq (create-useq* *kb-seq* sine 30 30000 0.2 0)))
  (setf (osc-freq sine) 440)
  (setf (osc-freq mod) 130)
  (setf (osc-init-ph sine) mod)
  (setf (unit-gain sine) 0.3)
  (pukunui:init seq 150))
