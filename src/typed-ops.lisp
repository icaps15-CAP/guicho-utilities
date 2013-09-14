
(in-package :guicho-utilities)
(use-syntax :annot)

@export
@doc "type: evaluated. @example (define-typed-op d+ + 'double-float)"
(defmacro define-typed-op (name op type
                           &key
                           (return-value-on t)
                           (input-value-on t))
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
;; @doc "defaulted to double-float."
;; (defvar *desired-type* 'double-float)

@export
@doc "the type of computation in lmates, defaulted to `double-float.'"
(deftype *desired-type* ()
  'double-float)

@export
@doc "coerce ARG into *DESIRED-TYPE* "
(defun desired (arg)
  (coerce arg '*desired-type*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ops


@export
@doc "typed +. see utility/typed-op"
(define-typed-op d+ + *desired-type*)
@export
@doc "typed *. see utility/typed-op"
(define-typed-op d* * *desired-type*)
@export
@doc "typed -. see utility/typed-op"
(define-typed-op d- - *desired-type*)
@export
@doc "typed /. see utility/typed-op"
(define-typed-op d/ / *desired-type*)
@export
@doc "typed sqrt. see utility/typed-op"
(define-typed-op dsqrt sqrt *desired-type*)


@export
@doc "typed aref. see utility/typed-op"
(define-typed-op daref aref *desired-type* :input-value-on nil)

@export
@doc "typed min. see utility/typed-op"
(define-typed-op dmin min *desired-type*)


@export
@doc "typed max. see utility/typed-op"
(define-typed-op dmax max *desired-type*)


@export
@doc "typed >. see utility/typed-op"
(define-typed-op d> > *desired-type* :return-value-on nil)

@export
@doc "typed <. see utility/typed-op"
(define-typed-op d< < *desired-type* :return-value-on nil)

@export
@doc "typed >=. see utility/typed-op"
(define-typed-op d>= >= *desired-type* :return-value-on nil)

@export
@doc "typed <. see utility/typed-op"
(define-typed-op d<= <= *desired-type* :return-value-on nil)


@export
@doc "typed =. see utility/typed-op"
(define-typed-op d= = *desired-type* :return-value-on nil)

@export
@doc "typed random. see utility/typed-op"
(define-typed-op drandom random *desired-type*)

@export
@doc "typed slot-value. see utility/typed-op"
(define-typed-op dslot-value slot-value *desired-type* :input-value-on nil)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dsetf setf *desired-type* :input-value-on nil)


@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dabs abs *desired-type* :return-value-on nil)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dplusp plusp *desired-type* :return-value-on nil)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dminusp minusp *desired-type* :return-value-on nil)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dzerop zerop *desired-type* :return-value-on nil)


@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dcos cos *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dsin sin *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dtan tan *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dcosh cosh *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dsinh sinh *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dtanh tanh *desired-type*)


@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dacos acos *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dasin asin *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op datan atan *desired-type*)


@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dexp exp *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dexpt expt *desired-type*)

@export
@doc "typed setf. see utility/typed-op"
(define-typed-op dlog log *desired-type*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; constants

@export
@doc "eps of double float"
(defconstant +eps+ 1.0d-10)
@export
@doc "delta for approximation"
(defparameter +delta+ 1.0d-2)
@export
@doc "around 6.28"
(defconstant +2pi+ (d* 2.0d0 pi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; misc

@export
@doc "approximated ="
(defun =~ (&rest args)
  (let ((x1 (first args))
        (x2 (second args)))
    (and (< (abs (- x2 x1)) +delta+)
         (or (null (third args))
             (apply #'=~ (cdr args))))))

@export
@doc "approximated ="
(defun d=~ (&rest args)
  (let ((x1 (first args))
        (x2 (second args)))
    @type *desired-type* x1
    @type *desired-type* x2
    (and (d< (dabs (d- x2 x1)) +delta+)
         (or (null (third args))
             (apply #'d=~ (cdr args))))))
