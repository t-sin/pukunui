(defpackage #:pukunui/units/oscillator
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/units/core)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+))
(in-package #:pukunui/units/oscillator)

(defunit sine (osc)
    (((freq :export) :default 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (sin (+ (osc-ph u) (calc-slot (osc-init-ph u))))))
    (incf (osc-ph u) (* (/ (calc-slot (sine-freq u)) +sample-rate+) PI))
    v))

(defunit tri (osc)
    (((freq :export) :defaut 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (tri (+ (osc-ph u) (calc-slot (osc-init-ph u))))))
    (incf (osc-ph u) (/ (calc-slot (tri-freq u)) +sample-rate+ 2))
    v))

(defunit saw (osc)
    (((freq :export) :defaut 440 :max 20000 :min 0.05 :step 0.01))
  (let ((v (saw (+ (osc-ph u) (calc-slot (osc-init-ph u))))))
    (incf (osc-ph u) (/ (calc-slot (saw-freq u)) +sample-rate+ 2))
    v))

(defunit pulse (osc)
    (((freq :export) :defaut 440 :max 20000 :min 0.05 :step 0.01)
     ((duty :export) :default 0.5 :max 1 :min 0 :step 0.01))
  (let ((v (pulse (+ (osc-ph u) (calc-slot (osc-init-ph u)))
                  (calc-slot (pulse-duty u)))))
    (incf (osc-ph u) (/ (calc-slot (pulse-freq u)) +sample-rate+ 2))
    v))
