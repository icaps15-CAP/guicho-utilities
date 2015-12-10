
(in-package :guicho-utilities)
(use-syntax :annot)


@export
(defmacro break+ (&rest args)
  (let* ((last-form (lastcar args))
         (last last-form)
         (butlast (butlast args)))
    (once-only (last)
      `(progn
         (break "~@{~a~2%~<;;~@; result:~4i~:@_~a~;~:>~2%~}"
                ,@(iter (for arg in butlast)
                        (collect `',arg)
                        (collect `(list ,arg)))
                ',last-form (list ,last))
         ,last))))


