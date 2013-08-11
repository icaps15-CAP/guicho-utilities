
(in-package :guicho-utilities)
(use-syntax :annot)

@export
(defmacro restart-bind* (bindings &body body)
  `(restart-bind (,(car bindings))
     ,(if (cdr bindings)
	  `(restart-bind* ,(cdr bindings)
	     ,@body)
	  body)))

;; (restart-bind* ((retry (lambda (c) (print :retry)))
;; 		(continue (lambda (c) (print :retry))))
;;   (error "error!"))

@export
(defmacro do-restart (bindings &body body)
  (with-gensyms (start)
    `(tagbody
	,start
	(block nil
	  (restart-bind
	      ,(mapcar
		(lambda (binding)
		  (destructuring-bind
			(name function . key-value-pair)
		      binding
		    (with-gensyms (rest)
		      `(,name (lambda (&rest ,rest)
				(prog1
				    (apply ,function ,rest)
				  (go ,start)))
			      ,@key-value-pair))))
		bindings)
	    ,@body)))))


;; (do-restart ((retry (lambda (c) (print :retry)))
;; 	     (continue (lambda (c) (print :retry))))
;;   (error "error!"))
