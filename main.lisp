(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/playinfo
        #:pukunui/unit

        #:pukunui/units/core
        #:pukunui/units/sample
        #:pukunui/units/oscillator

        #:pukunui/pcm)
  (:import-from #:queues
                #:make-queue
                #:qtop
                #:qpop
                #:qpush)
  (:export #:init
           #:start
           #:stop))
(in-package #:pukunui)

(defparameter *unit-graph* nil)
(defparameter *paconf* nil)
(defparameter *masterinfo* nil)

(defun calc-toplevel ()
  (multiple-value-bind (l r)
      (calc-unit *unit-graph* *masterinfo*)
    (update-masterinfo *masterinfo*)
    (values l r)))

(defparameter *sound-thread* nil)

(defparameter *ev* (read-wav (asdf:system-relative-pathname :pukunui "ev.wav")))
(defparameter *dr* (read-wav (asdf:system-relative-pathname :pukunui "dr.wav")))

;;; WIP
(defmacro defdevice (name (&rest params) (&rest units) methods)
  `(progn
     (defunit dname (,@params ,@units)
       ,(getf methods :proc))
     (defun create-dname (,@params)
       ,@(getf methods :init))))

(defunit useq (unit)
  (((seq :export) :default (make-queue :simple-cqueue))
   (pulse :default (create-pulse 440 0.25))
   (adsr :default (create-uadsr 0 100 100 1 100))
   (multi :default (create-umultiply)))
  (let ((timepos (masterinfo-timepos pinfo))
        (tick (masterinfo-tick pinfo))
        (ev (qtop (useq-seq u))))
    (multiple-value-bind (l r)
        (calc-unit (useq-multi u) pinfo)
      (when (and ev (timepos< (nth 0 ev) timepos))
        (ecase (nth 1 ev)
          (:on (setf (uadsr-state (useq-adsr u)) :a
                     (pulse-freq (useq-pulse u)) (nth 2 ev)
                     (uadsr-start (useq-adsr u)) tick))
          (:off (setf (uadsr-state (useq-adsr u)) :r
                      (uadsr-start (useq-adsr u)) tick)))
        (qpop (useq-seq u)))
      (values l r))))

(let* ((%seq `((,(make-timepos :bar 0 :beat 0 :pos 0) :on ,(note->freq 48))
               (,(make-timepos :bar 0 :beat 0 :pos 0.25) :off)
               (,(make-timepos :bar 0 :beat 1 :pos 0) :on ,(note->freq 46))
               (,(make-timepos :bar 0 :beat 1 :pos 0.25) :off)
               (,(make-timepos :bar 0 :beat 2 :pos 0) :on ,(note->freq 44))
               (,(make-timepos :bar 0 :beat 2 :pos 0.25) :off)
               (,(make-timepos :bar 0 :beat 3 :pos 0) :on ,(note->freq 44))
               (,(make-timepos :bar 0 :beat 3 :pos 0.25) :off)
               (,(make-timepos :bar 1 :beat 0 :pos 0) :on ,(note->freq 48))
               (,(make-timepos :bar 1 :beat 0 :pos 0.25) :off)
               (,(make-timepos :bar 1 :beat 1 :pos 0) :on ,(note->freq 46))
               (,(make-timepos :bar 1 :beat 1 :pos 0.25) :off)
               (,(make-timepos :bar 1 :beat 2 :pos 0) :on ,(note->freq 44))
               (,(make-timepos :bar 1 :beat 2 :pos 0.25) :off)
               (,(make-timepos :bar 1 :beat 3 :pos 0) :on ,(note->freq 44))
               (,(make-timepos :bar 1 :beat 3 :pos 0.25) :off)))
       (cq (make-queue :simple-cqueue))
       (seq (create-useq cq)))
  (let ((pulse (useq-pulse seq))
        (adsr (useq-adsr seq))
        (multi (useq-multi seq)))
    (loop :for e :in %seq :do (qpush cq e))
    (setf (unit-src multi) (vector pulse adsr))
    (defparameter *seq* seq)))

(defun init-ugraph ()
  (let ((umix (create-umix))
        (sine (create-sine 880)))
    (setf (unit-gain sine) 0.3)
    (setf (unit-src umix) (vector *ev* *dr*))
    (setf (unit-pan umix) (create-sine 10))
    (setf (clip-playing-p *ev*) t)
    (setf (clip-loop-p *ev*) t)
    (setf (clip-playing-p *dr*) t)
    (setf (clip-loop-p *dr*) t)
    (setf *unit-graph* (create-unit *seq* 0.5 0))))

(defun init ()
  (setf *masterinfo* (make-masterinfo))
  (setf (masterinfo-bpm *masterinfo*) 80)
  (init-ugraph))

(defun start ()
  (unless *sound-thread*
    (let ((th (bt:make-thread (pastart *masterinfo* #'calc-toplevel)
                              :name "pukunui-sound-thread")))
      (setf *sound-thread* th))))

(defun stop ()
  (bt:destroy-thread *sound-thread*)
  (setf *sound-thread* nil))
