(defpackage #:pukunui/portaudio
  (:use #:cl)
  (:export #:+frames-per-buffer+
           #:+sample-rate+

           #:make-paconf
           #:paconf-frames-per-buffer
           #:paconf-sample-rate
           #:make-paconf*

           #:start))
(in-package #:pukunui/portaudio)

(defparameter +frames-per-buffer+ 1024)
(defparameter +sample-rate+ 44100.0D0)

(defstruct paconf
  frames-per-buffer
  sample-rate)

(defun make-paconf* (&key (frames +frames-per-buffer+)
                          (samples +sample-rate+))
  (make-paconf :frames-per-buffer frames
               :sample-rate samples))

(defun start (paconf signal-fn)
  (let ((frames-per-buffer (paconf-frames-per-buffer paconf))
        (sample-rate (paconf-sample-rate paconf)))
    (pa:with-audio
        (pa:with-default-audio-stream (stream 0 2
                                              :frames-per-buffer frames-per-buffer
                                              :sample-format :float
                                              :sample-rate sample-rate)
          (let ((buffer (make-array (* 2 frames-per-buffer)
                                    :initial-element 0.0
                                    :element-type 'single-float)))
            (loop
              (loop
                :for n :from 0 :below frames-per-buffer
                :for tick := 0 :then (1+ tick)
                :do (multiple-value-bind (l r)
                        (funcall signal-fn)
                      (setf (aref buffer (* 2 n)) (coerce l 'single-float)
                            (aref buffer (1+ (* 2 n))) (coerce r 'single-float))))
              (pa:write-stream stream buffer)))))))
