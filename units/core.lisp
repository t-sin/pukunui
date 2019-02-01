(defpackage #:pukunui/units/core
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/timeinfo)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+))
(in-package #:pukunui/units/core)

(defunit unit ()
  (((src :export) :default nil)
   ((gain :export) :default 1 :max 1 :min 0)
   ((pan :export) :default 0 :max 1 :min -1))
  (multiple-value-bind (l r)
      #@unit-src
    (stereo-mix l r #@unit-gain #@unit-pan)))

(defunit uoffset (unit)
  (((value :export) :default 0))
  (let ((val #@uoffset-value))
    (multiple-value-bind (l r)
        #@unit-src
    (values (+ l val) (+ r val)))))

(defunit uamp (unit)
  (((value :export) :default 1))
  (let ((val #@uamp-value))
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
              (calc-unit u ti)
            (incf l l2)
            (incf r r2))
      :finally (return (stereo-mix l r #@unit-gain #@unit-pan)))))

(defunit uadsr (unit)
  ((state :default nil)
   ((start :export) :default 0)
   ((a :export) :default 100 :max 10000 :min 0.1 :step 0.01)
   ((d :export) :default 200 :max 10000 :min 0.1 :step 0.01)
   ((s :export) :default 0.5 :max 1 :min 0 :step 0.01)
   ((r :export) :default 400 :max 10000 :min 0.1 :step 0.01))
  (let* ((state #@uadsr-state)
         (eplaced (- (timeinfo-tick ti) #@uadsr-start)))
    (multiple-value-bind (s v)
        (adsr #@uadsr-a #@uadsr-d #@uadsr-s #@uadsr-r state eplaced)
      (unless (eq s state)
        (setf (uadsr-state u) s))
      (stereo-mix v v #@unit-gain #@unit-pan))))
