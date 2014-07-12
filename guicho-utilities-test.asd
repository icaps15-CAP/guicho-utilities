#|
  This file is a part of guicho-utilities project.
  Copyright (c) 2013 guicho (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage guicho-utilities-test-asd
  (:use :cl :asdf))
(in-package :guicho-utilities-test-asd)

(defsystem guicho-utilities-test
  :author "guicho"
  :license "LLGPL"
  :depends-on (:guicho-utilities
               :alexandria
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "guicho-utilities")
                 (:file :ask-for)
                 (:file :categorize)
                 (:file :combinations)
                 (:file :with-iter-array)
                 (:file :more-labels))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :guicho-utilities)"))
		    (asdf:clear-system c)))
