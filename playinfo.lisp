(defpackage #:pukunui/playinfo
  (:use #:cl
        #:pukunui/constants)
  (:export #:timepos
           #:make-timepos
           #:timepos-p
           #:timepos-bar
           #:timepos-beat
           #:timepos-pos
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

(defun update-tick (masterinfo)
  (let* ((sample-rate (masterinfo-sample-rate masterinfo))
         (bpm (masterinfo-bpm masterinfo))
         (timepos (masterinfo-timepos masterinfo))
         (measure (masterinfo-measure masterinfo))
         (diff-beat (/ (* 60 bpm) sample-rate)))
    (let* ((new-pos (+ (timepos-pos timepos) diff-beat))
           (new-beat (+ (timepos-beat timepos) (floor new-pos 1)))
           (new-bar (+ (timepos-bar timepos) (floor new-beat (measure-beat measure)))))
      (setf (timepos-pos timepos) (floor new-pos 1)
            (timepos-beat timepos) (floor new-beat (measure-note measure))
            (timepos-bar timepos) (floor new-bar (measure-beat measure)))))
    masterinfo)

(defun update-masterinfo (masterinfo)
  (incf (masterinfo-tick masterinfo))
  (update-tick masterinfo))
