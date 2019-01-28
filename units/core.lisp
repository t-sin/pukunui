(defpackage #:pukunui/units/core
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+))
(in-package #:pukunui/units/core)

(defunit unit ()
  (((src :export) :default 0 :max 1 :min -1)
   ((gain :export) :default 1 :max 1 :min 0)
   ((pan :export) :default 0 :max 1 :min -1))
  (multiple-value-bind (l r)
      #@unit-src
    (let* ((g #@unit-gain))
      (pan (gain l g)
           (gain r g)
           #@unit-pan))))

(defunit offset (unit)
  (((value :export) :default 0))
  (let ((val #@offset-value))
    (multiple-value-bind (l r)
        #@unit-src
    (values (+ l val) (+ r val)))))

(defunit amp (unit)
  (((value :export) :default 1))
  (let ((val #@amp-value))
    (multiple-value-bind (l r)
        #@unit-src
      (values (* l val) (* r val)))))

(defunit umix (unit)
  ()
  (let ((ulis #@unit-src))
    (loop
      :for u :across ulis
      :with l := 0
      :with r := 0
      :do (multiple-value-bind (l2 r2)
              (calc-unit u)
            (incf l l2)
            (incf r r2))
      :finally (return (values l r)))))
