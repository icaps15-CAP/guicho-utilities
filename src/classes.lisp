
(in-package :gu)
(use-syntax :annot)

@export
(defgeneric check-object-inherits-class (obj class))
(delegate-method check-object-inherits-class
				 (obj (class symbol (find-class class))))
(defmethod check-object-inherits-class (obj (class class))
  (if (member class
			  (class-precedence-list (class-of obj)))
	  t
	  (error "~t~a~% does not inherit ~%~t~a~% it should inherit one of these subclasses below: ~%~%~a"
			 obj class
			 (class-all-subclasses class))))

@export
(defgeneric class-all-subclasses (class))
(delegate-method class-all-subclasses ((obj t (class-of obj))))
(delegate-method class-all-subclasses ((class symbol (find-class class))))
(defmethod class-all-subclasses ((class class))
  (aif (class-direct-subclasses class)
	   (list class (mapcar #'class-all-subclasses it))
	   class))

@export
(defgeneric check-object-inherits-class-in-order
	(obj class-stronger class-weaker))
(delegate-method check-object-inherits-class-in-order
				 (obj 
				  (class-stronger symbol (find-class class-stronger))
				  class-weaker))
(delegate-method check-object-inherits-class-in-order
				 (obj 
				  class-stronger
				  (class-weaker symbol (find-class class-weaker))))
(defmethod check-object-inherits-class-in-order
	(obj (class-stronger class) (class-weaker class))
  (check-object-inherits-class obj class-stronger)
  (check-object-inherits-class obj class-weaker)
  (let ((lst (class-precedence-list (class-of obj))))
	(if (> (position class-stronger lst)
		   (position class-weaker lst))
		(error "class ~a comes before ~a in the class precedence list of the object: ~%~% ~a"
			   (class-name class-stronger)
			   (class-name class-weaker)
			   obj)
		t)))

@export
(defun check-object-inherits-class-in-orders (obj class-order)
  (check-object-inherits-class-in-order obj
										(first class-order)
										(second class-order))
  (when (third class-order)
	(check-object-inherits-class-in-orders obj (cdr class-order))))




@eval-always
@export
(defun class-slot-names (c)
  (ensure-finalized c)
  (mapcar #'slot-definition-name (class-slots c)))

@export
(defmacro define-constructor (class-spec)
  (call-define-constructor class-spec))

(defun call-define-constructor (class-spec)
  (check-type class-spec symbol)
  `(eval-when (:load-toplevel :execute)
     (compile ',class-spec
	      (let* ((c (find-class ',class-spec))
		     (keys (class-slot-names c)))
		`(lambda (&rest args &key ,@keys &allow-other-keys)
		   (declare (ignore ,@keys))
		   (apply #'make-instance ,c args))))
     ;; (setf (fdefinition ',class-spec)
     ;; 	   )
     ))

(defun form-reader-method (c name)
  (check-type name symbol)
  (with-gensyms (instance)
    `(progn
       (ensure-generic-function ',name)
       (defmethod ,name ((,instance ,(class-name c)))
	 (slot-value ,instance ',name)))))

@export
(defmacro define-readers (class-spec &rest slot-names)
  (let* ((c (find-class class-spec))
	 (truenames (class-slot-names c)))
    (if (null slot-names)
	(let ((names truenames))
	  `(list
	     ,@(mapcar (curry #'form-reader-method c)  names)))
	(let ((names (intersection truenames slot-names))
	      (invalid-names (set-difference slot-names truenames)))
	  `(progn
	     (warn "Following arguments weren't valid slot names ~
                    for ~a and so they are ignored: ~%~
                    ~4t~a" ',(class-name c) ',invalid-names)
	     (list ,@(mapcar (curry #'form-reader-method c)  names)))))))

(defun form-writer-method (c name)
  (check-type name symbol)
  (with-gensyms (instance new-value)
    `(progn
       (ensure-generic-function '(setf ,name))
       (defmethod (setf ,name) (,new-value (,instance ,(class-name c)))
	 (setf (slot-value ,instance ',name) ,new-value)))))

@export
(defmacro define-writers (class-spec &rest slot-names)
  (let* ((c (find-class class-spec))
	 (truenames (class-slot-names c)))
    (if (null slot-names)
	(let ((names truenames))
	  `(list
	     ,@(mapcar (curry #'form-writer-method c)  names)))
	(let ((names (intersection truenames slot-names))
	      (invalid-names (set-difference slot-names truenames)))
	  `(progn
	     (warn "Following arguments weren't valid slot names ~
                    for ~a and so they are ignored: ~%~
                    ~4t~a" ',(class-name c) ',invalid-names)
	     (list ,@(mapcar (curry #'form-writer-method c)  names)))))))

@export
(defmacro define-accessors (class-spec &rest slot-names)
  `(append
     (define-readers ,class-spec ,@slot-names)
     (define-writers ,class-spec ,@slot-names)))

@export
(defmacro define-class-utils (class-spec &rest slot-names)
  `(cons
    (define-constructor ,class-spec)
    (define-accessors ,class-spec ,@slot-names)))