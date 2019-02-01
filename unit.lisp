(defpackage #:pukunui/unit
  (:use #:cl)
  (:export #:slot-spec
           #:make-slot-spec
           #:slot-spec-p
           #:slot-spec-default
           #:slot-spec-max
           #:slot-spec-min
           #:slot-spec-step

           #:base-unit
           #:make-base-unit
           #:base-unit-p
           #:base-unit-id

           #:calc-unit
           #:calc-slot
           #:defunit
           #:u
           #:ti))
(in-package #:pukunui/unit)

(defparameter *unit-id* 0)
(defparameter *unit-proc-map* (make-hash-table :test 'eq))
(defparameter *unit-spec-map* (make-hash-table))
(defparameter *unit-map* (make-hash-table))

(defstruct slot-spec
  default max min step)

(defmethod print-object ((s slot-spec) stream)
  (format stream "#(:DEF ~s :MAX ~s :MIN ~s :BY ~s)"
          (slot-spec-default s) (slot-spec-max s) (slot-spec-min s) (slot-spec-step s)))

(defstruct base-unit
  id last-tick)

(defun make-key (u)
  (intern (symbol-name (type-of u)) :keyword))

(defun calc-unit (u ti)
  (let ((proc (gethash (make-key u) *unit-proc-map*)))
    (if (null proc)
        (error (format nil "proc for ~s is not defined." u))
        (funcall proc u ti))))

(defun calc-slot (s ti)
  (typecase s
    (base-unit (calc-unit s ti))
    (t s)))

(defun ref-reader (s c1 c2)
  (declare (ignore c1 c2))
  (let ((name (read s)))
    `(calc-slot (,name u) ti)))

(set-dispatch-macro-character #\# #\@ #'ref-reader)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-slotspec (spec)
    (let ((new-spec (copy-list '(:default 0 :max 1 :min 0 :step 0.01))))
      (loop
        :for (k v) :on spec :by #'cddr
        :do (setf (getf new-spec k) v))
      new-spec))

  (defun collect-slotdefs (defs)
    (let ((slot-names nil)
          (exported-names nil)
          (slot-specs nil))
      (dolist (def defs)
        (destructuring-bind (name . spec)
            def
          (if (listp name)
              (let ((name* (first name)))
                (push name* slot-names)
                (when (eq (second name) :export)
                  (push name* exported-names))
                (setf (getf slot-specs (intern (symbol-name name*) :keyword))
                      (make-slotspec spec)))
              (progn
                (push name slot-names)
                (setf (getf slot-specs (intern (symbol-name name) :keyword))
                      (make-slotspec spec))))))
      (values (nreverse slot-names)
              (nreverse exported-names)
              slot-specs)))

  (defun make-defstruct (name parent slot-names slot-specs)
    `(defstruct (,name (:include ,(if (null parent) 'base-unit parent)))
       ,@(mapcar (lambda (slot-name)
                   (let ((spec (getf slot-specs (intern (symbol-name slot-name) :keyword))))
                     `(,slot-name ,(getf spec :default))))
                 slot-names)))

  (defun make-constructor (const-name name export-slots)
    (let ((constructor-orig (intern (format nil "MAKE-~a" name)))
          (unit-id (intern (format nil "~a-ID" name)))
          ($unit (gensym "maker/u")))
      `(defun ,const-name (,@export-slots)
         (let ((,$unit (,constructor-orig)))
           (setf (,unit-id ,$unit)
                 (prog1 pukunui/unit::*unit-id*
                   (incf pukunui/unit::*unit-id*)))
           ,@(mapcar (lambda (slot-name)
                       (let ((accessor (intern (format nil "~a-~a" name slot-name))))
                       `(setf (,accessor ,$unit) ,slot-name)))
                     export-slots)
           (setf (gethash (base-unit-id ,$unit) pukunui/unit::*unit-map*) ,$unit)))))

  (defun make-proc (name body)
    `(flet ((proc-fn (u ti) (declare (ignorable u ti)) ,@body))
       (setf (gethash ,(intern (symbol-name name) :keyword)
                      pukunui/unit::*unit-proc-map*)
             #'proc-fn)))

  (defmacro defunit (name (&optional parent) slots &body body)
    (let ((constructor (intern (format nil "CREATE-~a" (symbol-name name))))
          (unit-p (intern (format nil "~a-P" (symbol-name name))))
          ($specs (gensym "defunit/specs")))
      (multiple-value-bind (slot-names export-slots slot-specs)
          (collect-slotdefs slots)
        `(progn
           ,(make-defstruct name parent slot-names slot-specs)
           ,(make-constructor constructor name export-slots)
           ,(make-proc name body)
           (let (,$specs)
             ,@(mapcar (lambda (slot-name)
                         (let ((slot-name* (intern (symbol-name slot-name) :keyword)))
                         `(setf (getf ,$specs ,slot-name*)
                                (make-slot-spec ,@(getf slot-specs slot-name*)))))
                       export-slots)
             (setf (gethash ,(intern (symbol-name name) :keyword) pukunui/unit::*unit-spec-map*)
                   ,$specs))
           (export '(,name ,constructor ,unit-p))
           (export ',(mapcar (lambda (n)
                               (intern (format nil "~a-~a" name (symbol-name n))))
                             slot-names)))))))
