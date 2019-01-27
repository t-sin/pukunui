(defpackage #:pukunui/units/clip
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/units/core)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+)
  (:export #:read-wav))
(in-package #:pukunui/units/clip)

(defun 32bit-to-int (lsb msb)
  (if (= #xA0 (logand #xA0 msb))
      (- #xA000 (+ (* msb #xFF) lsb))
      (+ (* msb #xFF) lsb)))

(defun int-to-float (i bits/sample)
  (float (/ i (expt 2 bits/sample))))

(defunit clip (unit)
  (((lbuf :export))
   ((rbuf :export))
   ((loop-p :export) :default nil)
   (playing-p :default t)
   (idx :default 0))
  (cond ((null (clip-playing-p u))
         (values 0 0))
        ((>= (clip-idx u) (length (clip-lbuf u)))
         (if (clip-loop-p u)
             (progn
               (setf (clip-idx u) 1)
               (values (svref (clip-lbuf u) 0)
                       (svref (clip-rbuf u) 0)))
             (progn
               (setf (clip-playing-p u) nil)
               (values 0 0))))
        (t (let ((l (svref (clip-lbuf u) (clip-idx u)))
                 (r (svref (clip-rbuf u) (clip-idx u))))
             (incf (clip-idx u))
             (values l r)))))

(defun read-wav (pathname)
  (let* ((wav (wav:read-wav-file pathname))
         (fmt-chunk (getf (find "fmt " wav :test #'string= :key (lambda (c) (getf c :chunk-id)))
                          :chunk-data))
         (+channels+ (getf fmt-chunk :number-of-channels))
         (bits/sample (getf fmt-chunk :significant-bits-per-sample))
         (dat-chunk (find "data" wav :test #'string= :key (lambda (c) (getf c :chunk-id))))
         (dat (getf dat-chunk :chunk-data)))
    (let (lbuf rbuf)
      (loop
        :for i :from 0 :below (length dat) :by 4
        :do (let ((l1 (aref dat (+ i 0)))
                  (l2 (aref dat (+ i 1)))
                  (r1 (aref dat (+ i 2)))
                  (r2 (aref dat (+ i 3))))
              (push (int-to-float (32bit-to-int l1 l2) bits/sample) lbuf)
              (push (int-to-float (32bit-to-int r1 r2) bits/sample) rbuf)))
      (create-clip (coerce (nreverse lbuf) 'simple-vector)
                   (coerce (nreverse rbuf) 'simple-vector) nil))))
