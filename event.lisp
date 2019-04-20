(defpackage #:pukunui/event
  (:use #:cl
        #:pukunui/unit
        #:pukunui/units/core
        #:pukunui/units/oscillator
        #:pukunui/playinfo)
  (:import-from #:queues
                #:make-queue
                #:qtop
                #:qpop
                #:qpush)
  (:export #:clip
           #:make-clip
           #:clip-p
           #:clip-start
           #:clip-len
           #:clip-events

           #:create-useq*))
(in-package #:pukunui/event)

(defstruct clip
  start len events)

;; (defevent name () &body body)

(defunit useq (unit)
  (((queue :export) :default (make-queue :simple-cqueue))
   (pulse :default (create-pulse 440 0.25))
   (adsr :default (create-uadsr 0 100 100 1 100))
   (multi :default (create-umultiply)))
  (let ((timepos (masterinfo-timepos pinfo))
        (tick (masterinfo-tick pinfo))
        (ev (qtop (useq-queue u))))
    (multiple-value-bind (l r)
        (calc-unit (useq-multi u) pinfo)
      (when (and ev (timepos< (nth 0 ev) timepos))
        (ecase (nth 1 ev)
          (:on (setf (uadsr-state (useq-adsr u)) :a
                     (pulse-freq (useq-pulse u)) (nth 2 ev)
                     (uadsr-start (useq-adsr u)) tick))
          (:off (setf (uadsr-state (useq-adsr u)) :r
                      (uadsr-start (useq-adsr u)) tick)))
        (qpop (useq-queue u)))
      (values l r))))

(defun create-useq* (init-seq)
  (let* ((cq (make-queue :simple-cqueue))
         (seq (create-useq cq))
         (pulse (useq-pulse seq))
         (adsr (useq-adsr seq))
         (multi (useq-multi seq)))
    (loop :for e :in init-seq :do (qpush cq e))
    (setf (unit-src multi) (vector pulse adsr))
    seq))
