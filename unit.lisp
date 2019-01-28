(defpackage #:pukunui/unit
  (:use #:cl)
  (:export #:slot
           #:make-slot
           #:slot-p
           #:slot-val
           #:slot-default
           #:slot-max
           #:slot-min
           #:slot-step

           #:base-unit
           #:make-base-unit
           #:base-unit-p
           #:base-unit-id

           #:calc-unit
           #:calc-slot
           #:defunit))
(in-package #:pukunui/unit)

(defparameter *unit-id* 0)
(defparameter *unit-proc-map* (make-hash-table :test 'eq))
(defparameter *unit-map* (make-hash-table))

(defstruct slot
  val default max min step)

(defstruct base-unit
  id)

(defun calc-unit (u)
  (let ((proc (gethash (intern (symbol-name (type-of u)) :keyword) *unit-proc-map*)))
    (if (null proc)
        (error (format nil "proc for ~s is not defined." u))
        (funcall proc u))))

(defun calc-slot (s)
  (etypecase s
    (slot (slot-val s))
    (base-unit (calc-unit s))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-slotspec (spec)
    (let ((new-spec '(:val 0 :default 0 :max 1 :min 0 :step 0.01)))
      (loop
        :for (k v) :on spec :by #'cddr
        :do (setf (getf new-spec k) v))
      new-spec))

  (defun collect-slotdefs (defs)
    (let ((slot-names nil)
          (exported-names nil)
          (mod-names nil)
          (slot-specs nil))
      (dolist (def defs)
        (destructuring-bind (name . spec)
            def
          (if (listp name)
              (progn
                (push (car name) slot-names)
                (when (member :export (cdr name))
                  (push (first name) exported-names))
                (when (member :mod (cdr name))
                  (push (first name) mod-names))
                (setf (getf slot-specs (first name)) (make-slotspec spec)))
              (progn
                (push name slot-names)
                (setf (getf slot-specs name) (make-slotspec spec))))))
      (values (nreverse slot-names)
              (nreverse exported-names)
              (nreverse mod-names)
              slot-specs)))

  (defmacro defunit (name (&optional parent) slots &body body)
    (let (($constructor (intern (format nil "CREATE-~a" (symbol-name name))))
          ($unit-p (intern (format nil "~a-P" (symbol-name name))))
          ($u (gensym "defunit/u")))
      (multiple-value-bind (slot-names export-slots mod-slots slot-specs)
          (collect-slotdefs slots)
        `(progn
           (defstruct (,name (:include ,(if (null parent)
                                            'base-unit
                                            parent)))
             ,@(mapcar (lambda (n)
                         `(,n ,(let ((spec (getf slot-specs n)))
                                  (if (member n mod-slots)
                                      `(make-slot ,@spec)
                                      (getf spec :default)))))
                       slot-names))
           (defun ,$constructor (,@export-slots)
             (let ((,$u (,(intern (format nil "MAKE-~a" name))
                         :id (prog1
                                 pukunui/unit::*unit-id*
                               (incf pukunui/unit::*unit-id*)))))
               ,@(mapcar (lambda (n)
                           (if (member n mod-slots)
                               `(setf (slot-val (,(intern (format nil "~a-~a" name n)) ,$u)) ,n)
                               `(setf (,(intern (format nil "~a-~a" name n)) ,$u) ,n)))
                         export-slots)
               (setf (gethash (base-unit-id ,$u) pukunui/unit::*unit-map*) ,$u)
               ,$u))
           (setf (gethash ,(intern (symbol-name name) :keyword)
                          pukunui/unit::*unit-proc-map*)
                 (lambda (,(intern "U"))
                   (declare (ignorable ,(intern "U")))
                   ,@body))
           (export '(,name ,$constructor ,$unit-p))
           (export ',(mapcar (lambda (n)
                               (intern (format nil "~a-~a" name (symbol-name n))))
                             slot-names)))))))
