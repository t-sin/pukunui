(defpackage #:pukunui/unit
  (:use #:cl)
  (:export #:unit
           #:make-unit
           #:unit-p
           #:unit-id
           #:unit-gain

           #:stereo
           #:make-stereo
           #:stereo-p
           #:stereo-pan

           #:gen
           #:make-gen
           #:gen-p
           #:gen-ph
           #:gen-init-phase
           #:gen-2
           #:make-gen-2
           #:gen-2-p
           #:gen-2-ph
           #:gen-2-init-phase

           #:osc
           #:make-osc
           #:osc-p
           #:osc-freq
           #:osc-2
           #:make-osc-2
           #:osc-2-p
           #:osc-2-freq))
(in-package #:pukunui/unit)

(defparameter *unit-id* 0)
(defparameter *unit-proc-map* (make-hash-table :test 'eq))
(defparameter *unit-map* (make-hash-table))

(defun proc-unit (u)
  (let ((proc (gethash (intern (symbol-name (type-of u)) :keyword) *unit-proc-map*)))
    (if (null proc)
        (error (format nil "proc for ~s is not defined." u))
        (funcall proc u))))

(defstruct slot
  val default max min step)

(defstruct base-unit
  id)

(defun calculate-slot (s)
  (if (slot-p s)
      (slot-val s)
      (proc-unit s)))

(defun collect-slotdefs (defs)
  (let ((slot-names nil)
        (def-names nil)
        (slot-specs nil))
    (dolist (def defs)
      (if (symbol-p def)
          :default-slot-spec
          :specified-slot-spec))))

(defmacro defunit (name slots &body body)
  (let ((proc (gensym))
        (constructor (intern (format nil "create-~a" (symbol-name name))))
        (unit-p (intern (format nil "~a-P" (symbol-name name)))))
    (multiple-value-bind (names slotnames slotspecs)
        (collect-slotdefs slots)
      `(progn
         (defstruct (,name (:include base-unit)) ,@slots)
         (defun ,constructor ())
         (setf (gethash ,(intern (symbol-name name) :keyword)
                        pukunui/unit::*unit-proc-map*)
               (lambda (,(intern "U"))
                 (declare (ignorable ,(intern "U")))
                 ,@body))
         (export ',constructor)
         (export ',unit-p)))))

(defstruct (unit (:include base-unit))
  gain)

(defstruct (stereo (:include unit))
  pan)

(defstruct (gen (:include unit))
  init-phase ph)

(defstruct (gen-2 (:include stereo))
  init-phase ph)

(defstruct (osc (:include gen))
  freq)

(defstruct (osc-2 (:include gen-2))
  freq)

;;;;
;; unit graph

(defun unit-add (u g))
(defun unit-del (u g))
(defun unit-ins (u g pos))
