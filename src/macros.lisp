
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
               (permutations-of args))))



@export
(defun permutations-of (seq)
  "returns a list of sequences.
   each sequence is a permutation of the original sequence.

Based on Steinhaus–Johnson–Trotter algorithm + Even's speedup

;; 1

;; 2 1
;; 1 2

;; 3 2 1  -- forward
;; 2 3 1
;; 2 1 3
;; 1 2 3  -- backward
;; 1 3 2
;; 3 1 2

"
  (etypecase seq
    (cons (%permutations-of-cons seq))))

(defun %permutations-of-cons (seq)
  (match seq
    ((list _)
     (list seq))
    ((list* head rest-seq)
     (let* ((len (length seq))
            (n (1- len))) ; n -- 2, length -- 3
       (labels 
           ((insert (subperm pos)
              (let (acc (%subperm subperm))
                (dotimes (i pos)        ; i = 0
                  (push (car %subperm) acc)
                  (setf %subperm (cdr %subperm)))
                (push head acc); pos = 1
                (dotimes (i (- n pos))  ; 2 - 1 = 1
                  (push (car %subperm) acc)
                  (setf %subperm (cdr %subperm)))
                (nreverse acc)))
                                        ;
            (forward (subperms pos acc) ; pos -- 0,1,2
              (ematch subperms
                ((cons subperm rest)
                 (if (< pos n)
                     (forward subperms (1+ pos)
                              (cons (insert subperm pos) acc))
                     (backward rest pos
                               (cons (insert subperm pos) acc))))
                (_ acc)))
            (backward (subperms pos acc)
              (ematch subperms
                ((cons subperm rest)
                 (if (< 0 pos)
                     (backward subperms (1- pos)
                               (cons (insert subperm pos) acc))
                     (forward rest pos
                              (cons (insert subperm pos) acc))))
                (_ acc))))
         (forward (%permutations-of-cons rest-seq) 0 nil))))))


;; less efficient, bad performance in both memory and speed
;; 
;; @export
;; (defun permutations-of (lst)
;;   (if (cdr lst)
;;       (iter appender
;;             (for after on lst)
;;             (for e = (car after))
;;             (for rest = (append before (cdr after)))
;;             (iter
;;               (for perm in (permutations-of rest))
;;               (in appender (collect (cons e perm))))
;;             (collect e into before))
;;       (list lst)))
;; 

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

