(defpackage #:pukunui/units/oscillator
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/units/core)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+))
(in-package #:pukunui/units/oscillator)

(defunit osc (unit)
  ((ph :val 0)
   ((init-ph :export) :val 0 :default 0)))

(defunit sine (osc)
  (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (sin (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (* (/ #@sine-freq +sample-rate+) PI))
    (values v v)))

(defunit tri (osc)
  (((freq :export) :defaut 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (tri (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (/ #@tri-freq +sample-rate+ 2))
    (values v v)))

(defunit saw (osc)
  (((freq :export) :defaut 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (saw (+ #@osc-ph #@osc-init-ph))))
    (incf (osc-ph u) (/ #@saw-freq +sample-rate+ 2))
    (values v v)))

(defunit pulse (osc)
  (((freq :export) :defaut 440 :max 20000 :min 0.05 :step 0.01)
   ((duty :export) :default 0.5 :max 1 :min 0 :step 0.01))
  (let ((v (pulse (+ #@osc-ph #@osc-init-ph) #@pulse-duty)))
    (incf (osc-ph u) (/ #@pulse-freq +sample-rate+ 2))
    (values v v)))
