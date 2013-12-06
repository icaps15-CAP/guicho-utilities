
(in-package :guicho-utilities)
(use-syntax :annot)


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


;; (do ((temp-one 1 (1+ temp-one))
;;       (temp-two 0 (1- temp-two)))
;;      ((> (- temp-one temp-two) 5) temp-one)) =>  4

@eval-always
(defun form-iteration2 (subscript array axis-number body)
  (destructuring-bind (sym &optional
                           limit1 limit2 (step 1))  subscript
    (let ((lower-limit (if limit2 limit1 0))
          (upper-limit (if limit2 limit2
                           (if limit1
                               limit1
                               `(array-dimension
                                 ,array ,axis-number)))))
      (once-only (lower-limit upper-limit step)
        `(locally
             (declare (type fixnum ,lower-limit ,upper-limit ,step))
           (do ((,sym ,lower-limit (+ ,sym ,step)))
               ((<= ,upper-limit ,sym))
             (declare (type fixnum ,sym))
             ,body))))))

@eval-always
(defun form-iter-array2 (subscripts array body)
  (iter (for subscript in (reverse subscripts))
        (for axis-number downfrom (1- (length subscripts)))
        (for inner-body previous formed-body initially body)
        (for formed-body = 
             (form-iteration2 subscript array axis-number inner-body))
        (finally (return formed-body))))


@eval-always
(defun ensure-subscripts (subscripts)
  (mapcar (lambda (sub)
            (typecase sub
              (symbol (list sub))
              (cons
               (assert (symbolp (car sub)) nil
                       "a binding ~a is illegal because its first argument~
                       is not a symbol." sub)
               sub)))
          subscripts))

@eval-always
@export
@doc "instance: symbol, 
subscripts:
 (symbol|(symbol upper-limit)
        |(symbol lower-limit upper-limit)
        |(symbol lower-limit upper-limit step))"
(defmacro with-iter-array ((instance &rest subscripts)
                           array &body body)
  (once-only (array)
    (let ((instance (or instance (gensym "INSTANCE")))
          (subs (ensure-subscripts subscripts)))
      (form-iter-array2
       subs
       array
       `(symbol-macrolet ((,instance (aref ,array ,@(mapcar #'car subs))))
          ,@body)))))

;; (with-iter-array (cell i j k) (make-array '(3 3 3))
;;   (print cell))

;; (let ((data (make-array '(3 3 3 10))))
;;   (with-iter-array (cell i (j 1 3) (k 3) (l 3 3 2)) data
;;     (print cell)))