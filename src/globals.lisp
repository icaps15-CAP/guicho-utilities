
(in-package :guicho-utilities)
(annot:enable-annot-syntax)

@export
(defparameter *main-thread-output* *standard-output*)

@export
(defvar *hash* (make-hash-table))

@export
(defun lookup (obj)
  (gethash obj *hash*))
