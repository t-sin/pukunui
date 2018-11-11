(defpackage #:pukunui
  (:use #:cl)
  (:export #:start-pa))
(in-package #:pukunui)

(defparameter +frames-per-buffer+ 8192)
(defparameter +sample-rate+ 44100.0D0)

(defstruct unit
  sources gain pan proc-fn conf)

(defun proc-pan (l r pan)
  (values (* l (- (/ pan 2.0) 0.5))
          (* r (+ (/ pan 2.0) 0.5))))

(defun proc-unit (unit tick)
  (let ((gain (unit-gain unit))
        (pan (unit-pan unit)))
    (multiple-value-bind (l r)
        (funcall (unit-proc-fn unit) unit tick)
      (proc-pan (* l gain) (* r gain) pan))))
              

(defun make-sine ()
  (let ((angle 0))
    (lambda (unit tick)
      (declare (ignorable unit tick))
      (let ((val (sin angle)))
        (incf angle (* (/ (unit-conf unit) +sample-rate+) PI))
        (values val val)))))

(defun make-mixer ()
  (lambda (unit tick)
    (declare (ignorable unit tick))
    (loop
      :for u :in (unit-sources unit)
      :with ml := 0
      :with mr := 0
      :do (multiple-value-bind (l r)
              (proc-unit u tick)
            (incf ml l)
            (incf mr r))
      :finally (return (values ml mr)))))

(defun make-wave-shaper (fn)
  (lambda (unit tick)
    (declare (ignorable unit tick))
    (multiple-value-bind (l r)
        (proc-unit (unit-sources unit) tick)
      (values (* (unit-conf unit) (funcall fn l))
              (* (unit-conf unit) (funcall fn r))))))

(defun make-delay (size)
  (let* ((len (floor (* +sample-rate+ size)))
         (lbuf (make-array len :initial-element 0.0))
         (rbuf (make-array len :initial-element 0.0))
         (head 0))
    (lambda (unit tick)
      (declare (ignorable unit tick))
      (multiple-value-bind (l r)
          (proc-unit (unit-sources unit) tick)
        (setf (aref lbuf head) l
              (aref rbuf head) r
              head (mod (1+ head) len))
        (let ((tail (mod (+ head (unit-conf unit)) len)))
          (values (+ l (* 0.2 (aref lbuf tail)))
                  (+ r (* 0.2 (aref rbuf tail)))))))))

(defparameter *unit-root*
  (make-unit :sources (make-unit :sources (make-unit :sources (list (make-unit :sources nil
                                                                               :gain 1 :pan 0
                                                                               :proc-fn (make-sine)
                                                                               :conf 440)
                                                                    (make-unit :sources nil
                                                                               :gain 1 :pan 0
                                                                               :proc-fn (make-sine)
                                                                               :conf 880))
                                                     :gain 1 :pan 0
                                                     :proc-fn (make-mixer)
                                                     :conf nil)
                                 :gain 0.6 :pan 0
                                 :proc-fn (make-wave-shaper (lambda (v) v))
                                 :conf 1)
             :gain 1 :pan 0
             :proc-fn (make-delay 2)
             :conf 4000))

(defun start-pa ()
  (pa:with-audio
    (pa:with-default-audio-stream (s 0 2
                                     :frames-per-buffer +frames-per-buffer+
                                     :sample-rate +sample-rate+)
      (let ((buffer (make-array (* 2 +frames-per-buffer+) :initial-element 0.0)))
        (loop
          (loop
            :for n :from 0 :below +frames-per-buffer+
            :do (multiple-value-bind (l r)
                    (proc-unit *unit-root* 0)
                  (setf (aref buffer (* 2 n)) l
                        (aref buffer (1+ (* 2 n))) r)))
          (pa:write-stream s buffer))))))
