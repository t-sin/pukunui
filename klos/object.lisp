(defpackage #:klos/object
  (:use #:cl)
  (:import-from #:klos/klass
                #:klass
                #:klass-name
                #:klass-slotmap
                #:find-klass
                #:find-klass-by-id)
  (:export #:instantiate))
(in-package #:klos/object)

(defstruct object
  (id 0 :type fixnum)
  (klass nil :type klass)
  (slots nil :type simple-vector))

;; TODO: `slots` should store parent klass slots

(defparameter *object-id* 0)

(defun %parse-initial-values (pairs klass)
  (let* ((slotmap (klass-slotmap klass))
         (slots (make-array (hash-table-count slotmap) :element-type t)))
    (loop
      :for (k v) :on pairs :by #'cddr
      :for idx = (gethash k slotmap)
      :do (when (null idx)
            (error "klass ~s doesn't have the slot '~s'."
                   (klass-name klass) k))
      :do (setf (svref slots idx) v))
    slots))

(defun instantiate (name &rest initial-values &key &allow-other-keys)
  (let ((k (find-klass name)))
    (when (null k)
      (error (format nil "there is no klass '~s'." name)))
    (make-object :id *object-id* :klass k
                 :slots (%parse-initial-values initial-values k))))
