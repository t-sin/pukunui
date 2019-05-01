(defpackage #:klos
  (:use #:cl)
  (:export #:make-klass
           #:find-klass
           #:find-klass-by-id))
(in-package #:klos)

;;;; Klasses
;;
;; (because the name `find-class` is predefined by CCL)

(defstruct slotdef
  (pred nil :type function))

(defstruct (klass (:constructor %make-klass))
  (id 0 :type fixnum)
  (name :nil :type keyword)
  (supers nil :type t)
  (slotmap nil :type hash-table)
  (slotdefs nil :type slotdef))

(defparameter *klass-limit* 2000)
(defparameter *klass-id* 0)
(defparameter *klass-name-table*
  (make-hash-table :test #'eq))
(defparameter *klass-table*
  (coerce (make-array *klass-limit* :element-type 'klass)
             'simple-vector))

(defun find-klass (name)
  (if (not (keywordp name))
         (error (format nil "name ~s must be a keyword." name))
         (let ((idx (gethash name *klass-name-table*)))
           (when idx
             (svref *klass-table* idx)))))

(defun find-klass-by-id (id)
  ;; TODO: declare inline
  (when (or (minusp id) (>= id *klass-limit*))
    (error (format nil "invalid id: ~s." id)))
  (svref *klass-table* id))

(defun %collect-supers (supers)
  (coerce (loop
            :for name :in supers
            :for k := (find-klass name)
            :when (null k)
            :do (error (format nil "super klass ~s is not found." name))
            :collect k)
          'simple-vector))

(defun %parse-slotdef (slotdef)
  (unless (first slotdef)
    (error (format nil "slotdef must have a name.")))
  (let* ((name (first slotdef))
         (pred (and (cdr slotdef)
                    (getf (cdr slotdef) :pred))))
    (unless (keywordp name)
      (error (format nil "name ~s must be a keyword." name)))
    (unless (or (null pred) (functionp pred))
      (error (format nil ":pred must be a function.")))
    (values name
            (make-slotdef :pred pred))))

(defun %collect-slotdefs (slotdefs)
  (let ((smap (make-hash-table :test #'eq))
        (sdefs nil))
    (loop
      :for sd :in slotdefs
      :for idx := 0 :then (1+ idx)
      :do (multiple-value-bind (name sdef)
              (%parse-slotdef sd)
            (setf (gethash smap name) idx)
            (push sdef sdefs)))
    (values smap (coerce (nreverse sdefs) 'simple-vector))))

(defun make-klass (name supers slotdefs)
  (when (not (keywordp name))
    (error (format nil "name ~s must be a keyword." name)))
  (when (>= *klass-id* (length *klass-table*))
    (error (format nil "number of existing klass definition reached ~s." *klass-limit*)))
  (when (find-klass name)
    (error (format nil "class '~s' is already defined." name)))
  (multiple-value-bind (smap sdefs)
      (%collect-slotdefs slotdefs)
    (let ((c (%make-klass :id *klass-id*
                          :name (intern (symbol-name name) :keyword)
                          :supers (%collect-supers supers)
                          :slotmap smap
                          :slotdefs sdefs)))
      (setf (svref *klass-table* *klass-id*) c
            (gethash name *klass-name-table*) *klass-id*)
      (incf *klass-id*)
      (values c *klass-id*))))
