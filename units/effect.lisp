(defpackage #:pukunui/units/effect
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/playinfo
        #:pukunui/units/core)
  (:export #:create-delay*
           #:set-delay-1-src
           #:create-delay-1*))
(in-package #:pukunui/units/effect)

(defstruct ring-buffer
  buf pos)

(defmethod print-object ((o ring-buffer) stream)
  (format stream "#<rb :pos ~s>" (ring-buffer-pos o)))

(defun make-ring-buffer* (size)
  (make-ring-buffer :buf (make-array size)
                    :pos 0))

(defun rbref (rb idx)
  (let ((len (length (ring-buffer-buf rb))))
    (aref (ring-buffer-buf rb)
          (mod (+ len (- (ring-buffer-pos rb) (1+ idx)))
               len))))

(defun rbpush (v rb)
  (setf (aref (ring-buffer-buf rb) (ring-buffer-pos rb)) v)
  (setf (ring-buffer-pos rb)
        (mod (1+ (ring-buffer-pos rb))
             (length (ring-buffer-buf rb)))))

(defunit delay (unit)
  ((lbuf :default nil)
   (rbuf :default nil)
   (size :default 44100)
   ((tap :export) :default 0))
  (multiple-value-bind (l r)
      #@unit-src
    (rbpush l (delay-lbuf u))
    (rbpush r (delay-rbuf u))
    (values (rbref (delay-lbuf u) #@delay-tap)
            (rbref (delay-rbuf u) #@delay-tap))))

(defun create-delay* (&optional size)
  (let* ((delay (create-delay 0))
         (size (if size
                   size
                   (delay-size delay))))
    (setf (delay-lbuf delay) (make-ring-buffer* size))
    (setf (delay-rbuf delay) (make-ring-buffer* size))
    delay))

(defun update-taps (taps bufsize dtime pinfo)
  ;; dtimeに合わせてtapの位置を調整したいなあ
  taps)

(defunit delay-1 (unit)
  ((size)
   (time)
   (delay)
   (taps)
   ((mix :export) :default 0))
  (let* ((bufsize (delay-1-size u))
         (dtime (delay-1-time u))
         (delay (delay-1-delay u))
         (taps #@delay-1-taps)
         (mix #@delay-1-mix)
         (rdiff (/ (1+ (length taps)))))
    (multiple-value-bind (l r)
        #@unit-src
      (update-taps taps bufsize dtime pinfo)
      (rbpush l (delay-lbuf delay))
      (rbpush r (delay-rbuf delay))
      (loop
        :for tap :across taps
        :for ratio := (- 1 rdiff) :then (- ratio rdiff)
        :do (incf l (gain (rbref (delay-lbuf delay) tap) (* mix ratio)))
        :do (incf r (gain (rbref (delay-rbuf delay) tap) (* mix ratio))))
      (values l r))))

(defun create-delay-1* (time mix &optional (size (* 44100 10)))
  (let ((d (create-delay-1 mix))
        (taps (apply #'vector (loop :for n :from 0 :below 5 :collect (* 6500 n)))))
    (setf (delay-1-delay d) (create-delay* size))
    (setf (delay-1-taps d) taps)
    (setf (delay-1-time d) time)
    (setf (delay-1-size d) size)
    d))
