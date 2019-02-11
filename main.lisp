(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/playinfo
        #:pukunui/unit

        #:pukunui/units/core
        #:pukunui/units/clip
        #:pukunui/units/oscillator

        #:pukunui/pcm)
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

(defmacro defdevice ())

(defunit useq (unit)
  (((seq :export) :default nil)
   (saw :default (create-saw 440))
   (adsr :default (create-uadsr 0 4000 10000 0.2 5000))
   (multi :default (create-umultiply)))
  (let ((timepos (masterinfo-timepos pinfo))
        (tick (masterinfo-tick pinfo))
        (ev (car (useq-seq u))))
    (multiple-value-bind (l r)
        (calc-unit (useq-multi u) pinfo)
      (unless (null ev)
        (when (timepos< timepos (nth 0 ev))
          (case (nth 1 ev)
            (:on (setf (uadsr-state (useq-adsr u)) :a
                       (saw-freq (useq-saw u)) (nth 2 ev)
                       (uadsr-start (useq-adsr u)) tick))
            (:off (setf (uadsr-state (useq-adsr u)) :s
                        (uadsr-start (useq-adsr u)) tick)))
          (pop (useq-seq u))))
      (values l r))))

(let* ((seq `((,(make-timepos :bar 0 :beat 0 :pos 0) :on 440)
              (,(make-timepos :bar 0 :beat 0 :pos 0.5) :off)
              (,(make-timepos :bar 0 :beat 1 :pos 0) :on 880)
              (,(make-timepos :bar 0 :beat 1 :pos 0.5) :off)
              (,(make-timepos :bar 0 :beat 2 :pos 0) :on 440)
              (,(make-timepos :bar 0 :beat 2 :pos 0.5) :off)
              (,(make-timepos :bar 0 :beat 3 :pos 0) :on 880)
              (,(make-timepos :bar 0 :beat 3 :pos 0.5) :off)))
       (seq (create-useq seq)))
  (let ((saw (useq-saw seq))
        (adsr (useq-adsr seq))
        (multi (useq-multi seq)))
    (setf (unit-src multi) (vector saw adsr))
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
  (init-ugraph))

(defun start ()
  (unless *sound-thread*
    (let ((th (bt:make-thread (pastart *masterinfo* #'calc-toplevel)
                              :name "pukunui-sound-thread")))
      (setf *sound-thread* th))))

(defun stop ()
  (bt:destroy-thread *sound-thread*)
  (setf *sound-thread* nil))
