(defpackage #:pukunui/units/core
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+))
(in-package #:pukunui/units/core)

(defunit unit ()
  (((src :export) :default 0 :max 1 :min -1)
   ((gain :export) :default 1 :max 1 :min 0))
  (multiple-value-bind (l r)
      (calc-slot (unit-src u))
    (let* ((g (calc-slot (unit-gain u)))
           (l (gain l g))
           (r (gain r g)))
      (values l r))))

(defunit offset (unit)
  (((value :export) :default 0))
  (let ((val (calc-slot (offset-value u))))
    (multiple-value-bind (l r)
        (calc-slot (unit-src u))
    (values (+ l val) (+ r val)))))

(defunit amp (unit)
  (((value :export) :default 1))
  (let ((val (calc-slot (amp-value u))))
    (multiple-value-bind (l r)
        (calc-slot (unit-src u))
      (values (* l val) (* r val)))))
