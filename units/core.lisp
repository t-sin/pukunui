(defpackage #:pukunui/units/core
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal))
(in-package #:pukunui/units/core)

(defunit unit ()
    (((out :export) :default 0 :max 1 :min -1)
     ((gain :export) :default 1 :max 1 :min 0))
  (let ((v (gain (calc-slot (unit-out u))
                 (calc-slot (unit-gain u)))))
    (values v v)))

(defunit osc (unit)
    ((ph :val 0)
     ((init-ph :export) :val 0)))
