
(in-package :cl-user)
(defpackage guicho-utilities
  (:nicknames :gu)
  (:use :cl
        :cl-syntax
        :trivial-cltl2
        :anaphora
        :annot
        :annot.class
        :annot.eval-when
        :annot.doc
        :annot.slot
        :iterate
        :alexandria
        :closer-mop)
  (:shadowing-import-from :cl
                          :standard-generic-function
                          :defmethod
                          :defgeneric))