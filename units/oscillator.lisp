(defpackage #:pukunui/units/oscillator
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/playinfo
        #:pukunui/units/core))
(in-package #:pukunui/units/oscillator)

(defunit osc (unit)
  ((ph :default 0)
   ((init-ph :export) :default 0)))

(defunit rand (osc)
  ()
  (let ((v (- (random 2) 1)))
    (values v v)))

(defunit sine (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (sin (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (* (/ #@sine-freq (masterinfo-sample-rate pinfo)) PI))
    (stereo-mix v v #@unit-gain #@unit-pan)))

(defunit tri (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (tri (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (/ #@tri-freq (masterinfo-sample-rate pinfo) 2))
    (stereo-mix v v #@unit-gain #@unit-pan)))

(defunit saw (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (saw (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (/ #@saw-freq (masterinfo-sample-rate pinfo) 2))
    (stereo-mix v v #@unit-gain #@unit-pan)))

(defunit pulse (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01)
   ((duty :export) :default 0.5 :max 1 :min 0 :step 0.01))
  (let ((v (pulse (+ #@osc-ph #@osc-init-ph) #@pulse-duty)))
    (incf (osc-ph u) (/ #@pulse-freq (masterinfo-sample-rate pinfo) 2))
    (stereo-mix v v #@unit-gain #@unit-pan)))
