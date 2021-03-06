(defpackage #:pukunui
  (:use #:cl)
  (:export #:generate-toplevel))
(in-package #:pukunui)

(defparameter +frames-per-buffer+ 28192)
(defparameter +sample-rate+ 44100.0D0)

(defparameter *bpm* 120)

(defun bpm->sec (bpm)
  (/ bpm 60))

(defun bpm->tick (bpm)
  (* (bpm->sec bpm) +sample-rate+))

(defstruct unit
  sources gain pan proc-fn conf)

(defun proc-pan (l r pan)
  (values (* l (/ (- 1 pan) 2.0))
          (* r (/ (+ 1 pan) 2.0))))

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
            ;; modulation
            (multiple-value-bind (l r)
                (proc-unit src tick)
              (values (* l val)
                      (* r val))))))))

(defun make-square ()
  (let ((angle 0))
    (lambda (unit tick)
      (declare (ignorable unit tick))
      (let ((val (if (< angle 0.5) 1 -1))
            (src (unit-sources unit)))
        (setf angle (mod (+ angle (/ (unit-conf unit) +sample-rate+)) 1))
        (if (null src)
            (values val val)
            ;; modulation
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

(defun make-filter ()
  (let ((ilbuf (make-array 2 :initial-element 0.0))
        (irbuf (make-array 2 :initial-element 0.0))
        (olbuf (make-array 2 :initial-element 0.0))
        (orbuf (make-array 2 :initial-element 0.0)))
    (lambda (unit tick)
      (declare (ignorable unit tick))
      (let* ((src (unit-sources unit))
             (conf (unit-conf unit))
             (type (getf conf :type))
             (freq (getf conf :freq))
             ;; (gain (getf conf :gain))
             (q (getf conf :q)))
        (ecase type
          (:lp (let* ((w (/ (* 2 PI freq) +sample-rate+))
                      (sinw (sin w))
                      (cosw (cos w))
                      (a (/ sinw (* 2 q)))
                      (b0 (/ (- 1 cosw) 2))
                      (b1 (- 1 cosw))
                      (b2 b0)
                      (a0 (+ 1 a))
                      (a1 (* -2 cosw))
                      (a2 (- 1 a)))
                 (flet ((filter (v ibuf obuf)
                          (+ (* (/ b0 a0) v)
                             (* (/ b1 a0) (aref ibuf 0))
                             (* (/ b2 a0) (aref ibuf 1))
                             (- (* (/ a1 a0) (aref obuf 0)))
                             (- (* (/ a2 a0) (aref obuf 1))))))
                   (multiple-value-bind (l r)
                       (proc-unit src tick)
                     (let ((ol (filter l ilbuf olbuf))
                           (or (filter r irbuf orbuf)))
                       ;; (format t "~s ~s~%" ol or)
                       (setf (aref ilbuf 1) (aref ilbuf 0)
                             (aref ilbuf 0) l
                             (aref irbuf 1) (aref irbuf 0)
                             (aref irbuf 0) r
                             (aref olbuf 1) (aref olbuf 0)
                             (aref olbuf 0) ol
                             (aref orbuf 1) (aref orbuf 0)
                             (aref orbuf 0) or)
                       (values ol or)))))))))))

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
          (values (+ l (* 0.4 (aref lbuf tail)))
                  (+ r (* 0.4 (aref rbuf tail)))))))))

(defun generate-adsr (tick start-tick state conf)
  (let* ((eplaced (- tick start-tick))
         (a (getf conf :a))
         (d (getf conf :d))
         (s (getf conf :s))
         (r (getf conf :r)))
    (cond ((and (member state '(nil :a)) (< eplaced a))
           (values :a (/ eplaced a)))
          ((and (member state '(:a :d)) (< eplaced (+ a d)))
           (values :d (- 1 (* (- 1 s) (/ (- eplaced a) d)))))
          ((and (member state '(:d :s)) (>= eplaced (+ a d)))
           (values :s s))
          (t (values nil 0)))))

(defun make-drum-machine (sequence)
  (let ((start 0)
        (note 0)
        (state :off))
    (lambda (unit tick)
      (declare (ignorable unit tick))
      ;; note on
      (when (= (mod tick (floor (* +sample-rate+ (/ 60 *bpm*)))) 0) ;; wait
        (when (nth note sequence) 
          (setf state :on
                start tick))
        (setf note (mod (1+ note) (length sequence))))
      ;; note off
      (when (and (eq state :on)
                 (= (- tick start) (floor (+ start (* +sample-rate+ 0.031)))))
        (setf state :off))
      (if (eq state :on)
          (values 1.0 1.0)
          (values 0.0 0.0)))))

(defparameter *sequencer* (make-unit :sources  (make-unit :sources :nil
                                                          :gain 1 :pan 0
                                                          :proc-fn (make-drum-machine '(t nil t nil t nil t t))
                                                          :conf '(:a 3000 :d 2500 :s 0.021 :r 100))
                                     :gain 1 :pan 0
                                     :proc-fn (make-square)
                                     :conf 440))

(defparameter *unit-root*
  (make-unit :sources (make-unit :sources *sequencer*
                                 :gain 0.7 :pan 0
                                 :proc-fn (make-delay 3)
                                 :conf 10000)
             :gain 1 :pan 0
             :proc-fn (make-filter)
             :conf (list :type :lp :freq 500 :gain 0.5 :q 6)))

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
;;                                  :gain 1 :pan 0
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
