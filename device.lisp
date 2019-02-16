(defpackage #:pukunui/device
  (:use #:cl
        #:pukunui/unit)
  (:export #:defdevice))
(in-package #:pukunui/device)

;;; WIP
(defmacro defdevice (name (&rest params) (&rest units) methods)
  `(progn
     (defunit dname (,@params ,@units)
       ,(getf methods :proc))
     (defun create-dname (,@params)
       ,@(getf methods :init))))
