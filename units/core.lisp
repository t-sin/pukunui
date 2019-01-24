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
