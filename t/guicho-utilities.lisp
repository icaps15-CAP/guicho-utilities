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
    (is (typep cat '(array list (3))))))
