
(in-package :guicho-utilities)
(annot:enable-annot-syntax)


@eval-always
@export
(defmacro with-iter-array-row-major ((&optional instance i) array
				     &body body)
  (once-only (array)
    (unless i (setf i (gensym "I")))
    (unless instance (setf instance (gensym "INSTANCE")))
    `(dotimes (,i (array-total-size ,array))
       (declare (ignorable ,i))
       (symbol-macrolet ((,instance (row-major-aref ,array ,i)))
	 ,@body))))

(defun form-iteration (subscript array axis-number body)
  `(dotimes (,subscript (array-dimension ,array ,axis-number))
     (declare (ignorable ,subscript))
     ,body))

(defun form-iter-array (subscripts array body)
  (iter (for subscript in (reverse subscripts))
	(for axis-number downfrom (1- (length subscripts)))
	(for inner-body previous formed-body initially body)
	(for formed-body = 
	     (form-iteration subscript array axis-number inner-body))
	(finally (return formed-body))))

@eval-always
@export
(defmacro with-iter-array ((instance &rest subscripts)
			   array &body body)
  (once-only (array)
    (unless instance (setf instance (gensym "INSTANCE")))
    (form-iter-array
     subscripts
     array
     `(symbol-macrolet ((,instance (aref ,array ,@subscripts)))
	,@body))))
