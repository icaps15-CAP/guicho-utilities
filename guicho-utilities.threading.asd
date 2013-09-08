
(defsystem guicho-utilities.threading
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (:iterate
		:alexandria
		:cl-syntax-annot
                :inferior-shell
                :bordeaux-threads
                :lparallel)
  :components ((:module "src"
			:serial t   	
			:components
			((:file :package)
			 (:file :threads))))
  :description "Personal threading utilities for Masataro Asai(guicho2.71828@gmail.com).")