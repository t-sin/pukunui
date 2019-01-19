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

           #:gen-2
           #:make-gen-2
           #:gen-2-p
           #:gen-2-ph
           #:gen-2-init-phase

           #:simple-osc
           #:make-simple-osc
           #:simple-osc-p
           #:simple-osc-freq))
(in-package #:pukunui/unit)

;;;;
;; signal unit

(defstruct unit
  id gain)

(defmethod print-object ((unit unit) stream)
  (format stream "(:unit ~a)" (unit-gain unit)))

(defstruct (stereo (:include unit))
  pan)

(defmethod print-object ((stereo stereo) stream)
  (format stream "(:stereo ~a)" (stereo-pan stereo)))

(defstruct (gen (:include unit))
  init-phase ph)

(defstruct (gen-2 (:include stereo))
  init-phase ph)

(defmethod print-object ((gen-2 gen-2) stream)
  (format stream "(:gen-2 ~a)" (gen-2-init-phase gen-2)))

(defstruct (simple-osc (:include gen-2))
  freq)

;;;;
;; unit graph

(defun unit-add (u g))
(defun unit-del (u g))
(defun unit-ins (u g pos))
