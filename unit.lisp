(defpackage #:pukunui/unit
  (:cl #:cl)
  (:export #:unit
           #:make-unit
           #:unit-p
           #:unit-id
           #:unit-gain

           #:stereo
           #:make-stereo
           #:stereo-p
           #:stereo-pan

           #:osc
           #:make-osc
           #:osc-p
           #:osc-theta))
(in-package #:pukunui/unit)

;;;;
;; signal generator unit

(defstruct unit
  id gain)

(defmethod print-object ((unit unit) stream)
  (format stream "(:unit ~a)" (unit-gain unit)))

(defstruct (stereo (:include unit))
  pan)

(defmethod print-object ((stereo stereo) stream)
  (format stream "(:stereo ~a)" (stereo-pan stereo)))

;;;;
;; absctract oscillator

(defstruct osc
  init-phase ph)

(defmethod print-object ((osc osc) stream)
  (format stream "(:osc ~a)" (osc-init-phase osc)))

;;;;
;; unit graph

(defun unit-add (u g))
(defun unit-del (u g))
(defun unit-ins (u g pos))
