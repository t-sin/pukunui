(defpackage #:pukunui/playinfo
  (:use #:cl
        #:pukunui/constants)
  (:export #:time
           #:make-time
           #:time-p
           #:time-bar
           #:time-beat
           #:time-pos
           #:time->tick

           #:playback-mode
           #:masterinfo
           #:make-masterinfo
           #:masterinfo-p
           #:masterinfo-frames-per-buffer
           #:masterinfo-sample-rate
           #:masterinfo-bpm
           #:masterinfo-playback-mode
           #:masterinfo-tick

           #:update-masterinfo))
(in-package #:pukunui/playinfo)

(defstruct time
  bar beat pos)

(defun time= (t1 t2)
  t)

(defun time< (t1 t2)
  t)

(defun time->tick (time)
  0)

(deftype playback-mode ()
  '(member :timeline :clip))

(defstruct masterinfo
  (frames-per-buffer +frames-per-buffer+)
  (sample-rate +sample-rate+)
  (bpm +bpm+)
  (playback-mode :timeline :type 'playback-mode)
  (playing-p nil)
  ;; time
  (tick 0))

(defun update-masterinfo (masterinfo)
  (incf (masterinfo-tick masterinfo)))
