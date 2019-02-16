(defpackage #:pukunui/playinfo
  (:use #:cl
        #:pukunui/constants)
  (:export #:timepos
           #:make-timepos
           #:timepos-p
           #:timepos-bar
           #:timepos-beat
           #:timepos-pos
           #:timepos+
           #:timepos=
           #:timepos<
           #:time->tick

           #:playback-mode
           #:masterinfo
           #:make-masterinfo
           #:masterinfo-p
           #:masterinfo-frames-per-buffer
           #:masterinfo-sample-rate
           #:masterinfo-mode
           #:masterinfo-bpm
           #:masterinfo-timepos
           #:masterinfo-tick

           #:update-masterinfo))
(in-package #:pukunui/playinfo)

(defstruct timepos
  (bar 0)
  (beat 0)
  (pos 0))

(defstruct measure
  (beat 4)
  (note 4))

(defun timepos+ (t1 t2 measure)
  (let ((new-timepos (make-timepos)))
    (multiple-value-bind (quot new-pos)
        (floor (+ (timepos-pos t1) (timepos-pos t2)) 1)
      (setf (timepos-pos new-timepos) new-pos)
      (multiple-value-bind (quot new-beat)
          (floor (+ (timepos-beat t1) (timepos-beat t2) quot) (measure-note measure))
        (setf (timepos-beat new-timepos) new-beat
              (timepos-bar new-timepos) (+ (timepos-bar t1) (timepos-bar t2) quot))))
    new-timepos))

(defun timepos= (t1 t2)
  (and (= (timepos-bar t1) (timepos-bar t2))
       (= (timepos-beat t1) (timepos-beat t2))
       (= (timepos-pos t1) (timepos-pos t2))))

(defun timepos< (t1 t2)
  (or (< (timepos-bar t1) (timepos-bar t2))
      (and (= (timepos-bar t1) (timepos-bar t2))
           (< (timepos-beat t1) (timepos-beat t2)))
      (and (= (timepos-bar t1) (timepos-bar t2))
           (= (timepos-beat t1) (timepos-beat t2))
           (< (timepos-pos t1) (timepos-pos t2)))))

(defun timepos->tick (timepos masterinfo)
  0)

(deftype playback-mode ()
  '(and keyword (member :timeline :clip)))

(defstruct masterinfo
  (frames-per-buffer +frames-per-buffer+)
  (sample-rate +sample-rate+)
  (playback-mode :timeline :type 'playback-mode)
  (playing-p nil)
  (measure (make-measure))
  (bpm +bpm+)
  (timepos (make-timepos) :type 'timepos)
  (tick 0))

(defun update-timepos (masterinfo)
  (let* ((sample-rate (masterinfo-sample-rate masterinfo))
         (bpm (masterinfo-bpm masterinfo))
         (timepos (masterinfo-timepos masterinfo))
         (measure (masterinfo-measure masterinfo))
         (diff-beat (/ bpm 60 sample-rate)))
    (multiple-value-bind (q new-pos)
        (floor (+ (timepos-pos timepos) diff-beat) 1)
      (setf (timepos-pos timepos) new-pos)
      (multiple-value-bind (q new-beat)
          (floor (+ (timepos-beat timepos) q) (measure-note measure))
        (setf (timepos-beat timepos) new-beat)
        (incf (timepos-bar timepos) q))))
  masterinfo)

(defun update-masterinfo (masterinfo)
  (incf (masterinfo-tick masterinfo))
  (update-timepos masterinfo))
