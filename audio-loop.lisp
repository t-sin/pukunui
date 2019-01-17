(defpackage #:pukunui/audio-loop
  (:use #:cl)
  (:import-from #:portaudio
                #:with-audio
                #:with-default-audio-stream
                #:write-stream)
  (:export #:+frames-per-buffer+
           #:+sample-rate+

           #:make-paconf
           #:paconf-frames-per-buffer
           #:paconf-sample-rate
           #:make-paconf*

           #:start))
(in-package #:pukunui/audio-loop)

(defparameter +frames-per-buffer+ 28192)
(defparameter +sample-rate+ 44100.0D0)

(defstruct paconf
  frames-per-buffer
  sample-rate)

(defun start (paconf)
(defun make-paconf* (&key (frames +frames-per-buffer+)
                          (samples +sample-rate+))
  (make-paconf :frames-per-buffer frames
               :sample-rate samples))

  (let ((frames-per-buffer (paconf-frames-per-buffer paconf))
        (sample-rate (paconf-sample-rate paconf)))
    (with-audio
        (with-default-audio-stream (stream 0 2
                                           :frames-per-buffer frames-per-buffer
                                           :sample-rate sample-rate)
          (let ((buffer (make-array (* 2 frames-per-buffer) :initial-element 0.0)))
            (loop
              (loop
                :for n :from 0 :below frames-per-buffer
                :for tick := 0 :then (1+ tick)
                :do (multiple-value-bind (l r)
                        (values 0 0)
                      (setf (aref buffer (* 2 n)) l
                            (aref buffer (1+ (* 2 n))) r)))
              (write-stream stream buffer)))))))
