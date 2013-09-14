

(defpackage guicho-utilities.threading
  (:use :cl
        :cl-syntax
        :annot
        :annot.class
        :annot.eval-when
        :annot.doc
        :annot.slot
        :bordeaux-threads
        :lparallel
        :inferior-shell))

(in-package :guicho-utilities.threading)
(use-syntax :annot)

@export
(defparameter *shared-output* *standard-output*)
@export
(defvar *print-lock* (make-lock "IO Stream lock"))

@export
(defvar *hash* (make-hash-table))

@export
(defun lookup (obj)
  (gethash obj *hash*))

@export
(defun get-core-num ()
  (length
   (run `(pipe (cat /proc/cpuinfo)
                              (grep processor))
        :output :lines)))

(setf lparallel:*kernel* (make-kernel (get-core-num)))
