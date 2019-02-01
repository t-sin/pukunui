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
   ((loop-p :export) :default nil)
   (playing-p :default t)
   (idx :default 0))
  (let ((idx #@clip-idx)
        (lbuf #@clip-lbuf)
        (rbuf #@clip-rbuf))
    (cond ((null (clip-playing-p u))
           (values 0 0))
          ((>= idx (length lbuf))
           (if (clip-loop-p u)
               (progn
                 (setf (clip-idx u) 1)
                 (stereo-mix (svref lbuf 0) (svref rbuf 0) #@unit-gain #@unit-pan))
               (progn
                 (setf (clip-playing-p u) nil)
                 (values 0 0))))
          (t (let ((l (svref lbuf idx))
                   (r (svref rbuf idx)))
               (incf (clip-idx u))
               (stereo-mix l r #@unit-gain #@unit-pan))))))

(defmethod print-object ((o clip) stream)
  (format stream "#(CLIP :LBUF ~a :RBUF ~a :LOOP-P ~a :PLAYING-P ~a)"
          (length (clip-lbuf o)) (length (clip-rbuf o)) (clip-loop-p o) (clip-playing-p o)))