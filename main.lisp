(defpackage #:pukunui
  (:use #:cl)
  (:export #:start-pa))
(in-package #:pukunui)

(defparameter +frames-per-buffer+ 8192)
(defparameter +sample-rate+ 44100.0D0)

(defparameter *bpm* 100)

(defun bpm->sec (bpm)
  (/ bpm 60))

(defun bpm->tick (bpm)
  (* (bpm->sec bpm) +sample-rate+))

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
      (let ((val (sin angle))
            (src (unit-sources unit)))
        (incf angle (* (/ (unit-conf unit) +sample-rate+) PI))
        (if (null src)
            (values val val)
            (multiple-value-bind (l r)
                (proc-unit src tick)
              (values (* l val)
                      (* r val))))))))

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

(defun make-drum-machine (sequence)
  (let ((start 0)
        (note 1)
        (state nil))
    (unless (= (length sequence) 16)
      (warn "sequence length is not 16."))
    (flet ((adsr (tick conf)
             (let* ((eplaced (- tick start))
                    (a (getf conf :a))
                    (d (getf conf :d))
                    (s (getf conf :s))
                    (r (getf conf :r)))
               (cond ((and (member state '(nil :a)) (< eplaced a))
                      (setf state :a)
                      (/ (- tick start) a))
                     ((and (member state '(:a :d)) (< eplaced (+ a d)))
                      (setf state :d)
                      (+ (/ (- eplaced a) d) s))
                     ((and (member state '(:d :s)) (> eplaced (+ a d)))
                      (setf state :s)
                      s)
                     ((and (eq state :r) (<= eplaced r))
                      (setf state :r)
                      (+ s (- 1 (/ eplaced r))))
                     (t
                      (setf state nil)
                      0)))))
      (lambda (unit tick)
        (declare (ignorable unit tick))
        (let ((dtick (* +sample-rate+ (/ 60 *bpm*))))
          (cond ((and (null state) (>= tick (mod tick (* note dtick))))
                 ;; note on
                 (let ((a-zero (zerop (getf (unit-conf unit) :a)))
                       (d-zero (zerop (getf (unit-conf unit) :d))))
                   (setf state (cond ((and a-zero d-zero) :s)
                                     (a-zero :d)
                                     (t :a))
                         start tick)))
                ((>= (- tick start) (* +sample-rate+ 0.25))
                 ;; note off
                 (setf state :r
                       start tick)))
          (setf note (mod note (1+ 16)))
          (let ((val (adsr tick (unit-conf unit))))
            (values val val)))))))

(defparameter *unit-root* (make-unit :sources  (make-unit :sources :nil
                                                          :gain 0.4 :pan 0
                                                          :proc-fn (make-drum-machine '())
                                                          :conf '(:a 500 :d 500 :s 1 :r 100))
                                     :gain 0.8 :pan 0
                                     :proc-fn (make-sine)
                                     :conf 440))

;; (defparameter *unit-root*
;;   (make-unit :sources (make-unit :sources (make-unit :sources (list (make-unit :sources *sequencer*
;;                                                                                :gain 1 :pan 0
;;                                                                                :proc-fn (make-sine)
;;                                                                                :conf 440)
;;                                                                     (make-unit :sources nil
;;                                                                                :gain 1 :pan 0
;;                                                                                :proc-fn (make-sine)
;;                                                                                :conf 880))
;;                                                      :gain 1 :pan 0
;;                                                      :proc-fn (make-mixer)
;;                                                      :conf nil)
;;                                  :gain 0.6 :pan 0
;;                                  :proc-fn (make-wave-shaper (lambda (v) v))
;;                                  :conf 1)
;;              :gain 1 :pan 0
;;              :proc-fn (make-delay 2)
;;              :conf 4000))

(defun start-pa ()
  (pa:with-audio
    (pa:with-default-audio-stream (s 0 2
                                     :frames-per-buffer +frames-per-buffer+
                                     :sample-rate +sample-rate+)
      (let ((buffer (make-array (* 2 +frames-per-buffer+) :initial-element 0.0)))
        (loop
          (loop
            :for n :from 0 :below +frames-per-buffer+
            :for tick := 0 :then (1+ tick)
            :do (multiple-value-bind (l r)
                    (proc-unit *unit-root* tick)
                  (setf (aref buffer (* 2 n)) l
                        (aref buffer (1+ (* 2 n))) r)))
          (pa:write-stream s buffer))))))
