
(in-package :cl-user)
(defpackage guicho-utilities
  (:use :cl
	:cl-syntax
	:trivial-cltl2
	:anaphora
	:annot
	:annot.class
	:annot.eval-when
	:annot.doc
	:annot.slot
	:trivial-timers
	:iterate
	:alexandria
	:closer-mop)
  (:shadow :standard-generic-function
	   :defmethod
	   :defgeneric))
