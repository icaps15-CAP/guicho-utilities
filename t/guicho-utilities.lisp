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

