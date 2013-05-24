
(in-package :guicho-utilities)
(use-syntax :annot)

@export
(defun make-interactive-handler (stream messages)
  (lambda ()
    (iter (for msg in messages)
	  (format stream "~%values already input: ~a~%~a > " vars msg)
	  (collecting (read) into vars)
	  (finally (return vars)))))

@export
(defun set-interactive-handler (name stream messages)
  (setf (symbol-function name)
	(make-interactive-handler stream messages)))

@eval-always
@export
(defmacro define-interactive-handler (name stream &body messages)
  `(set-interactive-handler ',name ,stream (list ,@messages)))

@export
(defun make-change-value-reporter (obj reader-fn message)
  (lambda (s)
    (format s "~a : ~a" message (funcall reader-fn obj))))

@export
(defun set-change-value-reporter (name obj reader-fn
				  &optional (message "Change value"))
  (setf (symbol-function name)
	(make-change-value-reporter obj reader-fn message)))
