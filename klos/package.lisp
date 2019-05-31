(defpackage #:klos
  (:use)
  (:import-from #:klos/class
                #:make-class
                #:find-class
                #:find-class-by-id)
  (:export #:make-class
           #:find-class
           #:find-class-by-id))
(in-package #:klos)
