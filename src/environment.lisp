

(in-package :guicho-utilities)
(use-syntax :annot)

@export
(defmacro print-environment (&environment env
			     &optional vars funs)
  (print env)
  (print (declaration-information 'optimize env))
  (print (declaration-information 'declaration env))
  (iter (for var in vars)
	(format t "~%~a : ~{~a ~}"
	 var
	 (multiple-value-list
	  (variable-information var env))))
  (iter (for fn in funs)
	(format t "~%~a : ~{~a ~}"
	 fn
	 (multiple-value-list
	  (function-information fn env))))
  (format t "~%")
  '(progn))
