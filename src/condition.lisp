
(in-package :guicho-utilities)
(use-syntax :annot)

@export
(defmacro restart-bind* (bindings &body body)
  `(restart-bind (,(car bindings))
     ,(if (cdr bindings)
          `(restart-bind* ,(cdr bindings)
             ,@body)
          body)))

@export
(defmacro handler-bind* (bindings &body body)
  `(handler-bind (,(car bindings))
     ,(if (cdr bindings)
          `(handler-bind* ,(cdr bindings)
             ,@body)
          body)))


@export
@doc "The variation of restart-case whose behavior is the same but
the semantics are that of RESTART-BIND.
Just as RESTART-CASE, the condition is handled first (that is, it jumps
out of the RESTART-BIND scope with GO) and then
the restart function is called. Finally, RESTART-RETURN returns
the value of restart function."
(defmacro restart-return (bindings &body body)
  (with-gensyms (block-name)
    (let ((bindings2
           (mapcar
            (lambda (binding)
              (destructuring-bind
                    (name function . key-value-pair)
                  binding
                (with-gensyms (fn-name rest)
                  (list `(,fn-name
                          (&rest ,rest)
                          (return-from ,block-name
                            (apply #',function ,rest)))
                        `(,name (lambda (&rest ,rest)
                                  (apply #',fn-name ,rest))
                                ,@key-value-pair)))))
            bindings)))
      `(block ,block-name
         (flet ,(mapcar #'first bindings2)
           (restart-bind
               ,(mapcar #'second bindings2)
             ,@body))))))

@export
@doc "The variation of handler-case whose behavior is the same but
the semantics are that of HANDLER-BIND.
Just as HANDLER-CASE, the condition is handled first (that is, it jumps
out of the HANDLER-BIND scope with GO) and then
the handler function is called. Finally, HANDLER-RETURN returns
the value of handler function."
(defmacro handler-return (bindings &body body)
  (with-gensyms (block-name)
    (let ((bindings2
           (mapcar
            (lambda (binding)
              (destructuring-bind
                    (name function . key-value-pair)
                  binding
                (with-gensyms (fn-name rest)
                  (list `(,fn-name
                          (&rest ,rest)
                          (return-from ,block-name
                            (apply #',function ,rest)))
                        `(,name (lambda (&rest ,rest)
                                  (apply #',fn-name ,rest))
                                ,@key-value-pair)))))
            bindings)))
      `(block ,block-name
         (flet ,(mapcar #'first bindings2)
           (handler-bind
               ,(mapcar #'second bindings2)
             ,@body))))))


;; (restart-return ((retry (lambda (c) (print :retry)))
;;               (continue (lambda (c) (print :retry))))
;;   (error "error!"))

;; (restart-case
;;     (error "error!")
;;   (retry (c) (print :retry))
;;   (continue (c) (print :retry)))

;; (restart-bind* ((retry (lambda (c) (print :retry)))
;;              (continue (lambda (c) (print :retry))))
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
;;           (continue (lambda (c) (print :retry))))
;;   (error "error!"))
