
(in-package :guicho-utilities)
(annot:enable-annot-syntax)

@eval-always
(defvar *optimization-hash-table*
  (make-hash-table))

@eval-always
@export
(defun package-optimize-setting (&optional
				 (speed 0)
				 (debug 3)
				 (safety 3)
				 (space 3))
  (setf (gethash *package*
		 *optimization-hash-table*)
	(list speed debug safety space)))

@eval-always
@export
(defmacro optimize* ()
  (destructuring-bind (speed debug safety space)
      (gethash *package*
	       *optimization-hash-table*
	       (list 0 3 3 3))
    `(declaim (optimize (debug ,debug) (safety ,safety)
			(space ,space) (speed ,speed)))))


@eval-always
@export
(defmacro speed* ()
  '(declaim (optimize (debug 0) (safety 0) (space 0) (speed 3))))


