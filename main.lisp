(defpackage #:pukunui
  (:use #:cl
        #:pukunui/portaudio
        #:pukunui/signal
        #:pukunui/unit)
  (:export #:generate-toplevel))
(in-package #:pukunui)

;; (defparameter *unit-graph* nil)

;; (defun proc-osc-2 (gen2)
;;   (let ((v (tri (gen-2-ph gen2))))
;;     (incf (gen-2-ph gen2) (/ (simple-osc-freq gen2) +sample-rate+))
;;     (multiple-value-bind (l r)
;;         (gain-2 v v (unit-gain gen2))
;;       (pan l r (stereo-pan gen2)))))

;; (defun generate-toplevel ()
;;   (proc-osc *unit-graph*))

;; ;; test
;; (setf *unit-graph* (make-osc-1 :id 0 :gain 0.4 :pan 0.2 :init-phase 0 :ph 0 :freq 440))
