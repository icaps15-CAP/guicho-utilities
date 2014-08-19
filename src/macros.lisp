
(in-package :guicho-utilities)
(use-syntax :annot)

@eval-always
@export
@doc "define a method in which the call is delegated to another object."
(defmacro delegate-method
    (method-name (&rest var-class-form))
  `(defmethod ,method-name
       ,(mapcar (lambda (d)
                  (if (consp d)
                      (if (second d)
                          (list (first d)
                                (second d))
                          (first d))
                      d))
                var-class-form)
     (,method-name 
      ,@(mapcar (lambda (d)
                  (if (consp d)
                      (or (third d)
                          (first d))
                      d))
                var-class-form))))

@eval-always
@export
@doc "memoize certain value into the slot.
no care for multiple-evaluation."
(defmacro with-memoising-slot ((slot instance) &body body)
  `(if (slot-boundp ,instance ',slot)
       (slot-value ,instance ',slot)
       (setf (slot-value ,instance ',slot)
             (progn ,@body))))

@eval-always
@export
@doc "defines methods for each of the all possible permutation of
 the arguments. For example,

(define-permutation-methods a ((a lst) (b vector) c)
  (append a b))

is same as

(PROGN
 (DEFMETHOD A ((A LST) (B VECTOR) C) (APPEND A B))
 (DEFMETHOD A ((A LST) C (B VECTOR)) (APPEND A B))
 (DEFMETHOD A ((B VECTOR) (A LST) C) (APPEND A B))
 (DEFMETHOD A ((B VECTOR) C (A LST)) (APPEND A B))
 (DEFMETHOD A (C (A LST) (B VECTOR)) (APPEND A B))
 (DEFMETHOD A (C (B VECTOR) (A LST)) (APPEND A B)))

"
(defmacro define-permutation-methods (name args &body body)
  `(progn
     ,@(mapcar (lambda (arglst)
                 `(defmethod ,name ,arglst ,@body))
               (permutations args))))

@eval-always
@export
(defmacro bias-if (bias then &optional else)
  `(if (d< (drandom 1.0d0) ,bias)
       ,then
       ,else))

@eval-always
@export
(defmacro funcall-when (fun &rest args)
  (once-only (fun)
    `(when ,fun
       (funcall ,fun ,@args))))

@eval-always
@export
(defmacro funcall-when-or-pass (fun &rest args)
  (once-only (fun)
    `(if ,fun
         (funcall ,fun ,@args)
         t)))

@eval-always
@export
(defmacro alias (name original)
  (assert (symbolp name))
  (if (symbolp original)
      `(call-alias ',name (function ,original))
      (if (eq (car original) 'defun)
          (with-gensyms (funsym)
            `(let ((,funsym ,original))
               (call-alias ,name (symbol-function ,funsym)))))))


@export
(defun call-alias (name fn)
  (print name)
  (setf (symbol-function name)
        fn))

@export
(defmacro define-local-function (name args &body body)
  "Same as `defun', but also establishes a local function.
 The functions defined by this macro
 can be used in `more-labels' later.

Tips: Using local-function and more-labels, you can test a local definition.
This is a great advantage over the standard flet and labels.
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name ,args
       (error "~a was called outside more-labels macro!~& Args:~a"
              ',name (list ,@(mapcar (lambda (x) `(quote ,x)) args))))
     (setf (get ',name :local-function) '(,name ,args ,@body))))

@export
(defmacro more-labels ((&key) definitions &body body)
  "Enhanced version of `labels'. It can be used as labels, however it also
recognizes the local functions defined by `define-local-function'.
`definitions' are the standard `definitions' form available in `labels'
or a symbol denoting a local function.
"
  `(labels
       ,(mapcar
         (lambda (def)
           (etypecase def
             (symbol
              (get def :local-function))
             (cons def)))
         definitions)
     ,@body))

