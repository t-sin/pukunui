(defpackage #:klos
  (:use #:cl)
  (:import-from #:klos/klass
                #:make-klass
                #:find-klass
                #:find-klass-by-id)
  (:export #:make-klass
           #:find-klass
           #:find-klass-by-id))
(in-package #:klos)
