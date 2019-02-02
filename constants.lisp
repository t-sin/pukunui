(defpackage #:pukunui/constants
  (:use #:cl)
  (:export #:+frames-per-buffer+
           #:+sample-rate+
           #:+bpm+))
(in-package #:pukunui/constants)

(defparameter +frames-per-buffer+ 1024)
(defparameter +sample-rate+ 44100.0D0)
(defparameter +bpm+ 110)
