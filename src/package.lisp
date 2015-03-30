
(in-package :cl-user)
(defpackage guicho-utilities
  (:use :cl
        :cl-syntax
        :introspect-environment
        :anaphora
        :annot
        :annot.class
        :annot.eval-when
        :annot.doc
        :annot.slot
        :optima
        :iterate
        :alexandria
        :closer-mop)
  (:shadowing-import-from :cl
                          :standard-generic-function
                          :defmethod
                          :defgeneric
                          :standard-method
                          :standard-class))
