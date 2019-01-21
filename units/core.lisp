(defpackage #:pukunui/units/core
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+))
(in-package #:pukunui/units/core)

(defunit unit ()
    (((out :export) :default 0 :max 1 :min -1)
     ((gain :export) :default 1 :max 1 :min 0))
  (let ((v (gain (calc-slot (unit-out u))
                 (calc-slot (unit-gain u)))))
    (values v v)))

(defunit osc (unit)
    ((ph :val 0)
     ((init-ph :export) :val 0 :default 0)))

(defunit sine (osc)
    (((freq :export) :default 0.05 :max 20000 :min 0.05 :step 0.01))
  (let ((v (sin (+ (osc-ph u) (osc-init-ph u)))))
    (incf (osc-ph u) (* (/ (sine-freq u) +sample-rate+) PI))
    v))
