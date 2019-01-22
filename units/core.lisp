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
