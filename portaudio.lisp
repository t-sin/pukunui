(defpackage #:pukunui/portaudio
  (:use #:cl
        #:pukunui/masterinfo)
  (:export #:pastart))
(in-package #:pukunui/portaudio)

(defun pastart (masterinfo signal-fn)
  (lambda ()
    (let ((frames-per-buffer (masterinfo-frames-per-buffer masterinfo))
          (sample-rate (masterinfo-sample-rate masterinfo)))
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
                :do (multiple-value-bind (l r)
                        (funcall signal-fn)
                      (setf (aref buffer (* 2 n)) (coerce l 'single-float)
                            (aref buffer (1+ (* 2 n))) (coerce r 'single-float))))
              (pa:write-stream stream buffer))))))))
