(defpackage #:pukunui/unit
  (:use #:cl)
  (:export #:unit
           #:make-unit
           #:unit-p
           #:unit-id
           #:unit-gain

           #:stereo
           #:make-stereo
           #:stereo-p
           #:stereo-pan

           #:gen
           #:make-gen
           #:gen-p
           #:gen-ph
           #:gen-init-phase))
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

(defstruct gen
  init-phase ph)

(defmethod print-object ((gen gen) stream)
  (format stream "(:gen ~a)" (gen-init-phase gen)))

;;;;
;; unit graph

(defun unit-add (u g))
(defun unit-del (u g))
(defun unit-ins (u g pos))
