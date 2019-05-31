(defpackage #:klos/object
  (:use #:cl)
  (:export #:instantiate))
(in-package #:klos/object)

(defstruct object
  (id 0 :type fixnum)
  (class nil :type klos/class:class)
  (slots nil :type simple-vector))

;; TODO: `slots` should store parent class slots

(defparameter *object-id* 0)

(defun %parse-initial-values (pairs class)
  (let* ((slotmap (klos/class:class-slotmap class))
         (slots (make-array (hash-table-count slotmap) :element-type t)))
    (loop
      :for (k v) :on pairs :by #'cddr
      :for idx = (gethash k slotmap)
      :do (when (null idx)
            (error "class ~s doesn't have the slot '~s'."
                   (klos/class:class-name class) k))
      :do (setf (svref slots idx) v))
    slots))

(defun instantiate (name &rest initial-values &key &allow-other-keys)
  (let ((k (klos/class:find-class name)))
    (when (null k)
      (error (format nil "there is no class '~s'." name)))
    (make-object :id *object-id* :class k
                 :slots (%parse-initial-values initial-values k))))
