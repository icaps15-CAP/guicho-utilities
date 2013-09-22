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
