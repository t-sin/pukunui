(defpackage #:klos/class
  (:use)
  (:export #:class
           #:class-name
           #:class-slotmap
           #:class-slotdefs
           #:make-class
           #:find-class
           #:find-class-by-id))
(in-package #:klos/class)

(cl:defstruct slotdef
  (pred cl:nil :type cl:function))

;; (because the name `find-class` is predefined by CCL)
(cl:defstruct (class (:constructor %make-class))
  (id 0 :type cl:fixnum)
  (name cl:nil :type cl:keyword)
  (supers cl:nil :type cl:t)
  (slotmap cl:nil :type cl:hash-table)
  (slotdefs cl:nil :type cl:simple-vector))

(cl:defparameter *class-limit* 2000)
(cl:defparameter *class-id* 0)
(cl:defparameter *class-name-table*
  (cl:make-hash-table :test #'cl:eq))
(cl:defparameter *class-table*
  (cl:coerce (cl:make-array *class-limit* :element-type 'cl:class)
             'cl:simple-vector))

(cl:defun find-class (name)
  (cl:if (cl:not (cl:keywordp name))
         (cl:error (cl:format cl:nil "name ~s must be a keyword." name))
         (cl:let ((idx (cl:gethash name *class-name-table*)))
           (cl:when idx
             (cl:svref *class-table* idx)))))

(cl:defun find-class-by-id (id)
  ;; TODO: declare inline
  (cl:when (cl:or (cl:minusp id) (cl:>= id *class-limit*))
    (cl:error (cl:format cl:nil "invalid id: ~s." id)))
  (cl:svref *class-table* id))

(cl:defun %collect-supers (supers)
  (cl:coerce (cl:loop
               :for name :in supers
               :for k := (find-class name)
               :when (cl:null k)
               :do (cl:error (cl:format cl:nil "super class ~s is not found." name))
               :collect k)
             'cl:simple-vector))

(cl:defun %parse-slotdef (slotdef)
  (cl:unless (cl:first slotdef)
    (cl:error (cl:format cl:nil "slotdef must have a name.")))
  (cl:let* ((name (cl:first slotdef))
            (pred (cl:and (cl:cdr slotdef)
                          (cl:getf (cl:cdr slotdef) :pred))))
    (cl:unless (cl:keywordp name)
      (cl:error (cl:format cl:nil "name ~s must be a keyword." name)))
    (cl:unless (cl:or (cl:null pred) (cl:functionp pred))
      (cl:error (cl:format cl:nil ":pred must be a function.")))
    (cl:values name
               (make-slotdef :pred pred))))

(cl:defun %collect-slotdefs (slotdefs)
  (cl:let ((smap (cl:make-hash-table :test #'cl:eq))
           (sdefs cl:nil))
    (cl:loop
      :for sd :in slotdefs
      :for idx := 0 :then (cl:1+ idx)
      :do (cl:multiple-value-bind (name sdef)
              (%parse-slotdef sd)
            (cl:setf (cl:gethash smap name) idx)
            (cl:push sdef sdefs)))
    (cl:values smap (cl:coerce (cl:nreverse sdefs) 'cl:simple-vector))))

(cl:defun make-class (name supers slotdefs)
  (cl:when (cl:not (cl:keywordp name))
    (cl:error (cl:format cl:nil "name ~s must be a keyword." name)))
  (cl:when (cl:>= *class-id* (cl:length *class-table*))
    (cl:error (cl:format cl:nil "number of existing class definition reached ~s." *class-limit*)))
  (cl:when (find-class name)
    (cl:error (cl:format cl:nil "class '~s' is already defined." name)))
  (cl:multiple-value-bind (smap sdefs)
      (%collect-slotdefs slotdefs)
    (cl:let ((c (%make-class :id *class-id*
                             :name (cl:intern (cl:symbol-name name) :keyword)
                             :supers (%collect-supers supers)
                             :slotmap smap
                             :slotdefs sdefs)))
      (cl:setf (cl:svref *class-table* *class-id*) c
               (cl:gethash name *class-name-table*) *class-id*)
      (cl:incf *class-id*)
      (cl:values c *class-id*))))
