(defpackage #:pukunui/units/sample
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/units/core))
(in-package #:pukunui/units/sample)

(defunit sample (unit)
  (((lbuf :export))
   ((rbuf :export))
   ((loop-p :export) :default nil)
   (playing-p :default t)
   (idx :default 0))
  (let ((idx #@sample-idx)
        (lbuf #@sample-lbuf)
        (rbuf #@sample-rbuf))
    (cond ((null (sample-playing-p u))
           (values 0 0))
          ((>= idx (length lbuf))
           (if (sample-loop-p u)
               (progn
                 (setf (sample-idx u) 1)
                 (stereo-mix (svref lbuf 0) (svref rbuf 0) #@unit-gain #@unit-pan))
               (progn
                 (setf (sample-playing-p u) nil)
                 (values 0 0))))
          (t (let ((l (svref lbuf idx))
                   (r (svref rbuf idx)))
               (incf (sample-idx u))
               (stereo-mix l r #@unit-gain #@unit-pan))))))

(defmethod print-object ((o sample) stream)
  (format stream "#(sample :LBUF ~a :RBUF ~a :LOOP-P ~a :PLAYING-P ~a)"
          (length (sample-lbuf o)) (length (sample-rbuf o)) (sample-loop-p o) (sample-playing-p o)))