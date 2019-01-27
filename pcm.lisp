(defpackage #:pukunui/pcm
  (:use #:cl
        #:pukunui/units/clip)
  (:import-from #:bit-smasher
                #:bits->int
                #:int->bits)
  (:import-from #:bit-ops
                #:as-bitwise-operations
                #:and
                #:eqv)
  (:export #:read-wav))
(in-package #:pukunui/pcm)

(defun make-bitvec (n &optional w)
  (let* ((b (int->bits n))
         (pad (make-array (- w (length b)) :initial-element 0)))
    (concatenate `(bit-vector ,w) pad b)))

(defconstant +sign-bit-16+ (int->bits #x8000))

(defun 16bit->float (lsb msb bits/sample)
  (let ((b (make-bitvec (+ (ash msb 8) lsb) 16)))
    (float (/ (if (equal +sign-bit-16+ (bit-and +sign-bit-16+ b))
                  (- 1 (bits->int (bit-not b)))
                  (bits->int b))
              (expt 2 (1- bits/sample))))))

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
              (push (16bit->float l1 l2 bits/sample) lbuf)
              (push (16bit->float r1 r2 bits/sample) rbuf)))
      (create-clip (coerce (nreverse lbuf) 'simple-vector)
                   (coerce (nreverse rbuf) 'simple-vector) nil))))
