(defpackage #:pukunui/playinfo
  (:use #:cl
        #:pukunui/constants)
  (:export #:time
           #:make-time
           #:time-p
           #:time-bar
           #:time-beat
           #:time-pos
           #:time=
           #:time<
           #:time->tick

           #:playback-mode
           #:masterinfo
           #:make-masterinfo
           #:masterinfo-p
           #:masterinfo-frames-per-buffer
           #:masterinfo-sample-rate
           #:masterinfo-playback-mode
           #:masterinfo-bpm
           #:masterinfo-time
           #:masterinfo-tick

           #:update-masterinfo))
(in-package #:pukunui/playinfo)

(defstruct time
  (bar 0)
  (beat 0)
  (pos 0))

(defun time= (t1 t2)
  (and (= (time-bar t1) (time-bar t2))
       (= (time-beat t1) (time-beat t2))
       (= (time-pos t1) (time-pos t2))))

(defun time< (t1 t2)
  (or (< (time-bar t1) (time-bar t2))
      (and (= (time-bar t1) (time-bar t2))
           (< (time-beat t1) (time-beat t2)))
      (and (= (time-bar t1) (time-bar t2))
           (= (time-beat t1) (time-beat t2))
           (< (time-pos t1) (time-pos t2)))))

(defun time->tick (time masterinfo)
  0)

(deftype playback-mode ()
  '(member :timeline :clip))

(defstruct masterinfo
  (frames-per-buffer +frames-per-buffer+)
  (sample-rate +sample-rate+)
  (playback-mode :timeline :type 'playback-mode)
  (playing-p nil)
  (bpm +bpm+)
  (time (make-time) :type 'time)
  (tick 0))

(defun update-time (masterinfo)
  (let* ((sample-rate (masterinfo-sample-rate masterinfo))
         (bpm (masterinfo-bpm masterinfo)))
    masterinfo))

(defun update-masterinfo (masterinfo)
  (incf (masterinfo-tick masterinfo))
  (update-time masterinfo))
