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

(defparameter *bass-seq*
  (flet ((seq1 (bar beat n1 n2)
           (list (list (make-timepos :bar bar :beat beat :pos 0) :on (note->freq n1))
                 (list (make-timepos :bar bar :beat beat :pos 0.5) :on (note->freq n2))
                 (list (make-timepos :bar bar :beat beat :pos 0.75) :off)
                 (list (make-timepos :bar bar :beat (1+ beat) :pos 0) :on (note->freq n2))
                 (list (make-timepos :bar bar :beat (1+ beat) :pos 0.25) :off)
                 (list (make-timepos :bar bar :beat (1+ beat) :pos 0.5) :on (note->freq n2))
                 (list (make-timepos :bar bar :beat (1+ beat) :pos 0.75) :off)))
         (seq2 (bar beat n)
           (list (list (make-timepos :bar bar :beat beat :pos 0) :on (note->freq n))
                 (list (make-timepos :bar bar :beat beat :pos 0.25) :off)
                 (list (make-timepos :bar bar :beat beat :pos 0.5) :on (note->freq n))
                 (list (make-timepos :bar bar :beat beat :pos 0.75) :off)
                 (list (make-timepos :bar bar :beat (1+ beat) :pos 0) :on (note->freq n))
                 (list (make-timepos :bar bar :beat (1+ beat) :pos 0.25) :off)
                 (list (make-timepos :bar bar :beat (1+ beat) :pos 0.5) :on (note->freq n))
                 (list (make-timepos :bar bar :beat (1+ beat) :pos 0.75) :off))))
    (append (seq1 1 0 48 52) (seq2 1 2 52) (seq1 2 0 48 52) (seq2 2 2 52)
            (seq1 3 0 50 53) (seq1 3 2 43 47) (seq1 4 0 48 52) (seq2 4 2 52))))

(defparameter *snare-seq*
  `((,(make-timepos :bar 0 :beat 3 :pos 0) :on)
    (,(make-timepos :bar 0 :beat 3 :pos 0.5) :on)
    (,(make-timepos :bar 0 :beat 3 :pos 0.75) :on)
    ,@(loop
        :for n :from 1 :upto 4
        :append `(,@(loop
                      :for m :from 0 :upto 2
                      :append `((,(make-timepos :bar n :beat m :pos 0) :on)
                                (,(make-timepos :bar n :beat m :pos 0.5) :on)
                                (,(make-timepos :bar n :beat m :pos 0.75) :on)))
                  (,(make-timepos :bar n :beat 3 :pos 0) :on)
                  (,(make-timepos :bar n :beat 3 :pos 0.25) :on)
                  (,(make-timepos :bar n :beat 3 :pos 0.5) :on)
                  (,(make-timepos :bar n :beat 3 :pos 0.75) :on)))))

;; gb
(let* ((seq (create-useq* *gb-seq* 30 30000 0.2 0)))
  (pukunui:init (create-unit seq 0.2 0) 60))

;; kb
(let* ((seq (create-useq* *kb-seq* 30 30000 0.2 0)))
  (pukunui:init (create-unit seq 0.2 0) 150))

;; snare
(let* ((seq (create-unseq* *snare-seq* 30 3000 0 0)))
  (pukunui:init (create-unit seq 0.2 0) 150))

;; kb2
(let* ((seq1 (create-useq* *kb-seq* 30 30000 0.2 0))
       (seq2 (create-useq* *bass-seq* 30 10000 0.5 0))
       (seq3 (create-unseq* *snare-seq* 30 3000 0 0))
       (mixer (create-umix)))
  (setf (unit-src mixer) (vector seq1 seq2 seq3))
  (pukunui:init (create-unit mixer 0.2 0) 150))
