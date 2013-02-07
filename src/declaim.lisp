
(in-package :guicho-utilities)
(annot:enable-annot-syntax)

@eval-always
@export
(defmacro optimize* ()
  '(declaim (optimize (debug 3) (safety 3) (space 0) (speed 0))))


@eval-always
@export
(defmacro speed* ()
  '(declaim (optimize (debug 0) (safety 0) (space 0) (speed 3))))
