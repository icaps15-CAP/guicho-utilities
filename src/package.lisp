
(in-package :cl-user)
(use-syntax :annot)
  (:use :cl
	:cl-syntax
	:anaphora
	:annot
	:annot.class
	:annot.eval-when
	:annot.doc
	:annot.slot
	:trivial-timers
	:iterate
	:alexandria)
  (:shadowing-import-from
   :closer-mop
   :class-slots
   :class-precedence-list
   :class-direct-subclasses
   :slot-definition-name
   ))


