#|
  This file is a part of guicho-utilities project.
  Copyright (c) 2013 guicho (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage guicho-utilities-test
  (:use :cl
        :alexandria
        :guicho-utilities
        :fiveam))
(in-package :guicho-utilities-test)

(def-suite :guicho-utilities)
(in-suite :guicho-utilities)

(test :with-iter-array
  (let ((a #2A((0 0 0 1 0)
               (0 1 0 1 0)
               (1 1 1 0 0)))
        results)
    (with-iter-array (n i j) a
      (when (plusp n)
        (push (list i j) results)))
    (is (set-equal '((0 3) (1 1) (1 3) (2 0) (2 1) (2 2))
                   results
                   :test #'equal))))

(test :with-iter-array-row-major
  (let ((a #2A((0 0 0 1 0)
               (0 1 0 1 0)
               (1 1 1 0 0)))
        results)
    (with-iter-array-row-major (n i) a
      (when (plusp n)
        (push i results)))
    (is (set-equal '(3 6 8 10 11 12)
                   results))))


(test :categorize-by-equality
  (let (cat)
    (finishes
      (setf cat (categorize-by-equality
                 (iota 10)
                 (lambda (a b)
                   (= (mod a 3)
                      (mod b 3))))))
    (print cat)
    (is (= 3 (length cat))))

  ;; intransitive
  (flet ((next-to (a b) (or (= (1+ a) b) (= (1+ b) a))))
    (let (cat)
      (finishes
        (setf cat (categorize-by-equality
                   (shuffle (append (iota 10) (iota 10 :start 20)))
                   #'next-to)))
      (print '(:transitive t))
      (print cat)
      (is-false (= 2 (length cat)))

      (finishes
        (setf cat (categorize-by-equality
                   (shuffle (append (iota 10) (iota 10 :start 20)))
                   #'next-to
                   :transitive nil)))
      (print '(:transitive nil))
      (print cat)
      (is-true (= 2 (length cat))))))


(test :permutations
  (is (set-equal '((0 1 2) (1 0 2) (1 2 0) (2 1 0) (2 0 1) (0 2 1))
                 (permutations '(0 1 2))
                 :test #'equal))
  (is (set-equal '((0 1) (0 2)
                   (1 0) (1 2)
                   (2 0) (2 1))
                 (permutations '(0 1 2) :length 2)
                 :test #'equal)))

(test :combinations
  (is (equal '(())
             (combinations '(0 1 2) :length 0)))
  (is (equal '((0) (1) (2))
             (combinations '(0 1 2) :length 1)))
  (is (equal '((0 1) (0 2) (1 2))
             (combinations '(0 1 2) :length 2)))
  (is (equal '((0 1 2))
             (combinations '(0 1 2) :length 3)))
  (is (equal '((0 1 2))
             (combinations '(0 1 2)))))


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
