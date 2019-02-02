(defpackage #:pukunui/masterinfo
  (:use #:cl
        #:pukunui/constants)
  (:export #:playback-mode
           #:masterinfo
           #:make-masterinfo
           #:masterinfo-p
           #:masterinfo-frames-per-buffer
           #:masterinfo-sample-rate
           #:masterinfo-bpm
           #:masterinfo-playback-mode
           #:masterinfo-tick

           #:update-masterinfo))
(in-package #:pukunui/masterinfo)

(deftype playback-mode ()
  '(member :timeline :clip))

(defstruct masterinfo
  (frames-per-buffer +frames-per-buffer+)
  (sample-rate +sample-rate+)
  (bpm +bpm+)
  (playback-mode :timeline :type 'playback-mode)
  (tick 0))

(defun update-masterinfo (masterinfo)
  (incf (masterinfo-tick masterinfo)))
