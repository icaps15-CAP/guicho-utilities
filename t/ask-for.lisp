(in-package :guicho-utilities-test)
(in-suite :guicho-utilities)

;; this is a basic usage of `ask-for'.
(defun fn1 (&optional (arg (ask-for arg 1 :in fn1)))
  (print arg))
(defun fn2 ()
  (loop for i from 0 to 10 collect (fn1)))
(defun fn3 ()
  (let ((lst (list :a :b :c)))
    (setf (cdddr lst) lst)
    (handler-bind ((ask-value
                    (lambda (c)
                      (when (and (eq 'arg (asked-name c))
                                 (eq 'fn1 (asked-by c)))
                        (use-value (pop lst) c)))))
      (fn2))))


;; Using `ask-for' is better than using a special variable, because
;; the generator and iterator of *queue* is in the different position
(defvar *queue*)
(defun fn-special1 ()
  (print (pop *queue*)))
(defun fn-special2 ()
  (loop for i from 0 to 10 collect (fn-special1)))
(defun fn-special3 ()
  (let ((*queue* (list :a :b :c)))
    (setf (cdddr *queue*) *queue*)
    (fn-special2)))


;; how about implementing with thunk and continuation?
(defvar *cont*)
(defun fn-cont1 ()
  (print (funcall *cont*)))
(defun fn-cont2 ()
  ;; In this case, someone may change the definition of `*cont*'. This is unsafe in
  ;; terms of debuggability.  And the effect continues until the let binding is
  ;; unwound. Actually, the behavior of fn-cont2-2 is affected by fn-cont2.  So the
  ;; timing of unwinding should be carefully considered during debugging.
  (setf *cont* (lambda () 1))
  (loop for i from 0 to 10 collect (fn-cont1)))
(defun fn-cont2-2 ()
  (loop for i from 0 to 3 collect (fn-cont1)))
(defun fn-cont3 ()
  (let* ((queue (list :a :b :c))
         (*cont* (lambda () (pop queue))))
    (setf (cdddr queue) queue)
    (fn-cont2-2)
    (fn-cont2)
    (fn-cont2-2)))

;; by using restarts, we can restrict the communication stream between upper stack
;; and lower stack. Restart bindings are safe because they cannot be modified -- it
;; can only be wrapped around.

;; now this is an exteded usage with a macro `in-reply-to'.

(defun fn3-in-reply-to ()
  (let ((lst (list :a :b :c)))
    (setf (cdddr lst) lst)
    (in-reply-to
        (((arg :in fn1) (pop lst))
         ((arg2 :in fn1) 1)
         (arg2 (pop lst)))
      (fn2))))


(test ask-for
  (is (= 1 (fn1)))
  (is (equal '(:a :b :c :a :b :c :a :b :c :a :b) (fn3)))
  (is (equal '(:a :b :c :a :b :c :a :b :c :a :b) (fn3-in-reply-to))))
