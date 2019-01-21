(defpackage #:pukunui/units/core
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal))
(in-package #:pukunui/units/core)

(defunit unit
    (((v :export) :default 0 :max 1 :min -1)
     ((gain :export) :default 1 :max 1 :min 0))
  (let ((v (gain (calc-slot (unit-v u))
                 (calc-slot (unit-gain u)))))
    (values v v)))

(defunit osc
    ((ph :val 0)
     ((init-ph :export) :val 0))
  (let ((v (sin (+ (osc-init-ph u) (osc-ph u)))))
    (incf (osc-ph u) 0.05)
    v))
