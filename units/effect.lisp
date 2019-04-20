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
   ((tap :export) :default 0))
  (multiple-value-bind (l r)
      #@unit-src
    (rbpush l (delay-lbuf u))
    (rbpush r (delay-rbuf u))
    (values (rbref (delay-lbuf u) #@delay-tap)
            (rbref (delay-rbuf u) #@delay-tap))))

(defun create-delay* (size)
  (let ((delay (create-delay 0)))
    (setf (delay-lbuf delay) (make-ring-buffer* size))
    (setf (delay-rbuf delay) (make-ring-buffer* size))
    delay))

(defunit delay-1 (unit)
  ((size)
   (delays)
   (dnum)
   ((mix :export) :default 0))
  (let ((size (delay-1-size u))
        (dlis #@delay-1-delays)
        (dnum #@delay-1-dnum)
        (mix #@delay-1-mix))
    (loop
      :for d :across dlis
      :for n :from 0 :upto dnum
      :with l := 0
      :with r := 0
      :do (setf (delay-tap d) (* n (/ size dnum)))
      :do (multiple-value-bind (l2 r2)
              (calc-unit d pinfo)
            (incf l (gain l2 (* mix (/ n dnum))))
            (incf r (gain r2 (* mix (/ n dnum)))))
      :finally (return (values l r)))))

(defun set-delay-1-src (delay-1 src)
  (let ((delays (delay-1-delays delay-1)))
    (loop
      :for d :across delays
      :do (setf (unit-src d) src))))

(defun create-delay-1* (size tapnum mix)
  (let ((d (create-delay-1 mix))
        (delays (apply #'vector
                       (loop
                         :for n :from 0 :below tapnum
                         :collect (create-delay* size)))))
    (setf (delay-1-delays d) delays)
    (setf (delay-1-dnum d) tapnum)
    (setf (delay-1-size d) size)
    d))
