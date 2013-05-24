
(in-package :guicho-utilities)
(use-syntax :annot)


@export
(defun break+ (&rest args)
  (break "~{~a~%~}" args))

@export
(defun break* (&rest args)
  (iter (for arg in args)
		(break "~a" arg)))

@eval-always
@export
(defmacro with-profiling ((&key packages) &body body)
  `(progn
     ,@(iter (for package in packages)
	     (collect `(swank::profile-package ,package t t)))
     (unwind-protect
	  ,@body
       (swank::profile-report)
       (swank::unprofile-all))))

@eval-always
@export
(defmacro with-tracing ((&rest symbols) &body body)
  `(progn
     (trace ,@symbols)
     (unwind-protect
	  ,@body
       (untrace ,@symbols))))
