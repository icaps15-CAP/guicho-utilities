

(in-package :guicho-utilities)
(use-syntax :annot)

@export
(defclass recursive-print-mixin () ())

(defmethod print-object ((o recursive-print-mixin) stream)
  (print-unreadable-object (o stream :type t)
	(format stream
			"~<~@{~{~a: ~a~}~^~_~}~:>"
			(mapcar (lambda (name)
					  (list name
							(if (slot-boundp o name)
								(slot-value o name)
								:unbound)))
					(mapcar #'slot-definition-name
							(class-slots
							 (class-of o)))))))


@export
@doc "make a shallow-copy of the original instance. Since the
reference for each object remains the same you should take care about
the side effects caused by modification to the slots in either
instacne. ARGS specifies initialization arguments to reinitialize-instance."
(defgeneric shallow-copy (obj &rest args &key &allow-other-keys))
(defmethod shallow-copy ((n number) &rest args &key &allow-other-keys)
  @ignore args
  n)
(defmethod shallow-copy ((s sequence) &rest args &key &allow-other-keys)
  @ignore args
  (copy-seq s))
(defmethod shallow-copy ((s array) &rest args &key &allow-other-keys)
  @ignore args
  (copy-array s))
(defmethod shallow-copy ((s hash-table) &rest args &key &allow-other-keys)
  @ignore args
  (copy-hash-table s))
(defmethod shallow-copy ((o standard-object)
						 &rest args &key &allow-other-keys)
  (let* ((class (class-of o))
		 (new (allocate-instance class)))
	(mapc
	 (lambda (name)
	   (ignore-errors
		 (setf (slot-value new name)
			   (slot-value o name))))
	 (mapcar #'slot-definition-name
			 (class-slots class)))
	(apply #'reinitialize-instance new args)
	new))

@export
(defclass deep-copyable () ())

@export
@doc "deep copy the object. It recursively copies only those
instances whose classes are the subclass of `deep-copyable'."
(defgeneric deep-copy (obj &rest args &key &allow-other-keys))
(defmethod deep-copy (o &rest args &key &allow-other-keys) o)

(defmethod deep-copy ((o deep-copyable) &rest args &key &allow-other-keys)
  (let* ((class (class-of o))
		 (new (allocate-instance class)))
	(mapc
	 (lambda (name)
	   (ignore-errors
		 (setf (slot-value new name)
			   (deep-copy (slot-value o name)))))
	 (mapcar #'slot-definition-name
			 (class-slots class)))
	(apply #'reinitialize-instance new args)
	new))
