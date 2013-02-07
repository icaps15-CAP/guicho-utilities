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
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "guicho-utilities"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
