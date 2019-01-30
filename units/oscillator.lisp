(defpackage #:pukunui/units/oscillator
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/units/core)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+))
(in-package #:pukunui/units/oscillator)

(defunit osc (unit)
  ((ph :default 0)
   ((init-ph :export) :default 0)))

(defunit sine (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (sin (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (* (/ #@sine-freq +sample-rate+) PI))
    (pan v v #@unit-pan)))

(defunit tri (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (tri (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (/ #@tri-freq +sample-rate+ 2))
    (pan v v #@unit-pan)))

(defunit saw (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (saw (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (/ #@saw-freq +sample-rate+ 2))
    (pan v v #@unit-pan)))

(defunit pulse (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01)
   ((duty :export) :default 0.5 :max 1 :min 0 :step 0.01))
  (let ((v (pulse (+ #@osc-ph #@osc-init-ph) #@pulse-duty)))
    (incf (osc-ph u) (/ #@pulse-freq +sample-rate+ 2))
    (pan v v #@unit-pan)))
