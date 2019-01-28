(defpackage #:pukunui/units/clip
  (:use #:cl
        #:pukunui/unit
        #:pukunui/signal
        #:pukunui/units/core)
  (:import-from #:pukunui/portaudio
                #:+sample-rate+)
  (:export #:read-wav))
(in-package #:pukunui/units/clip)

(defunit clip (unit)
  (((lbuf :export))
   ((rbuf :export))
   ((loop-p :export :mod) :default nil)
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

(defmethod print-object ((o clip) stream)
  (format stream "#<CLIP :LBUF ~a :RBUF ~a :LOOP-P ~a :PLAYING-P ~a>"
          (length (clip-lbuf o)) (length (clip-rbuf o)) (clip-loop-p o) (clip-playing-p o)))