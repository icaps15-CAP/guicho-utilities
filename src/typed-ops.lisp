
(in-package :guicho-utilities)
(use-syntax :annot)

@export
(defmacro define-typed-op (name op type
                           &key
                           (return-value-on t)
                           (input-value-on t))
  "type: evaluated. @example (define-typed-op d+ + 'double-float)"
  `(defmacro ,name (&rest args)
     (let ((the-args
            ,(if input-value-on
                 `(mapcar #'(lambda (argsym)
                              `(the ,',type ,argsym))
                          args)
                 `args)))
       ,(if return-value-on
            ``(the ,',type
                (,',op ,@the-args))
            ``(,',op ,@the-args)))))

;; @eval-always
;; @export
;; (defvar *desired-type* 'double-float)

@export
(deftype *desired-type* ()
  "the type of computation in lmates, defaulted to `double-float.'"
  'double-float)

@export
(defun desired (arg)
  "coerce ARG into *DESIRED-TYPE* "
  (coerce arg '*desired-type*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ops


@export
(define-typed-op d+ + *desired-type*)
@export
(define-typed-op d* * *desired-type*)
@export
(define-typed-op d- - *desired-type*)
@export
(define-typed-op d/ / *desired-type*)
@export
(define-typed-op dsqrt sqrt *desired-type*)


@export
(define-typed-op daref aref *desired-type* :input-value-on nil)

@export
(define-typed-op dmin min *desired-type*)


@export
(define-typed-op dmax max *desired-type*)


@export
(define-typed-op d> > *desired-type* :return-value-on nil)

@export
(define-typed-op d< < *desired-type* :return-value-on nil)

@export
(define-typed-op d>= >= *desired-type* :return-value-on nil)

@export
(define-typed-op d<= <= *desired-type* :return-value-on nil)


@export
(define-typed-op d= = *desired-type* :return-value-on nil)

@export
(define-typed-op drandom random *desired-type*)

@export
(define-typed-op dslot-value slot-value *desired-type* :input-value-on nil)

@export
(define-typed-op dsetf setf *desired-type* :input-value-on nil)


@export
(define-typed-op dabs abs *desired-type* :return-value-on nil)

@export
(define-typed-op dplusp plusp *desired-type* :return-value-on nil)

@export
(define-typed-op dminusp minusp *desired-type* :return-value-on nil)

@export
(define-typed-op dzerop zerop *desired-type* :return-value-on nil)


@export
(define-typed-op dcos cos *desired-type*)

@export
(define-typed-op dsin sin *desired-type*)

@export
(define-typed-op dtan tan *desired-type*)

@export
(define-typed-op dcosh cosh *desired-type*)

@export
(define-typed-op dsinh sinh *desired-type*)

@export
(define-typed-op dtanh tanh *desired-type*)


@export
(define-typed-op dacos acos *desired-type*)

@export
(define-typed-op dasin asin *desired-type*)

@export
(define-typed-op datan atan *desired-type*)


@export
(define-typed-op dexp exp *desired-type*)

@export
(define-typed-op dexpt expt *desired-type*)

@export
(define-typed-op dlog log *desired-type*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; constants

@export
(defconstant +eps+ 1.0d-10 "eps of double float")
@export
(defparameter +delta+ 1.0d-2 "delta for approximation")
@export
(defconstant +2pi+ (d* 2.0d0 pi) "around 6.28")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; misc

@export
(defun =~ (&rest args)
  "approximated ="
  (let ((x1 (first args))
        (x2 (second args)))
    (and (< (abs (- x2 x1)) +delta+)
         (or (null (third args))
             (apply #'=~ (cdr args))))))

@export
(defun d=~ (&rest args)
  "approximated ="
  (let ((x1 (first args))
        (x2 (second args)))
    @type *desired-type* x1
    @type *desired-type* x2
    (and (d< (dabs (d- x2 x1)) +delta+)
         (or (null (third args))
             (apply #'d=~ (cdr args))))))
