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

           #:create-useq*
           #:create-unseq*))
(in-package #:pukunui/event)

(defstruct clip
  start len events)

;; (defevent name () &body body)

(defunit useq (unit)
  (((queue :export) :default (make-queue :simple-cqueue))
   (osc :default nil)
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
                     (osc-freq (useq-osc u)) (nth 2 ev)
                     (uadsr-start (useq-adsr u)) tick))
          (:off (setf (uadsr-state (useq-adsr u)) :r
                      (uadsr-start (useq-adsr u)) tick)))
        (qpop (useq-queue u)))
      (values l r))))

(defun create-useq* (init-seq osc a d s r)
  (let* ((cq (make-queue :simple-cqueue))
         (seq (create-useq cq))
         (adsr (useq-adsr seq))
         (multi (useq-multi seq)))
    (loop :for e :in init-seq :do (qpush cq e))
    (setf (useq-osc seq) osc)
    (setf (uadsr-a adsr) a)
    (setf (uadsr-d adsr) d)
    (setf (uadsr-s adsr) s)
    (setf (uadsr-r adsr) r)
    (setf (unit-src multi) (vector osc adsr))
    seq))
