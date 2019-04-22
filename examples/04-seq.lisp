;; (ql:quickload :pukunui)

(use-package '(:pukunui/pcm
               :pukunui/playinfo
               :pukunui/signal
               :pukunui/units/core
               :pukunui/units/oscillator
               :pukunui/units/effect
               :pukunui/units/sample
               :pukunui/event))

(defparameter *gb-seq*
  `((,(make-timepos :bar 0 :beat 0 :pos 0) :on ,(note->freq 96))
    (,(make-timepos :bar 0 :beat 0 :pos 0.1) :on ,(note->freq 108))
    (,(make-timepos :bar 0 :beat 1 :pos 0) :off)))

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
    (,(make-timepos :bar 2 :beat 1 :pos 0) :on ,(note->freq 83))
    (,(make-timepos :bar 2 :beat 1 :pos 0.5) :on ,(note->freq 81))
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

;; GameBoy
(let* ((seq (create-useq* *gb-seq* 30 30000 0.2 0)))
  (pukunui:init (create-unit seq 0.2 0) 60))

;; Kirby 1
(let* ((seq (create-useq* *kb-seq* 30 30000 0.2 0)))
  (pukunui:init (create-unit seq 0.2 0) 150))
