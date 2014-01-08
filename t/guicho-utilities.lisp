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

