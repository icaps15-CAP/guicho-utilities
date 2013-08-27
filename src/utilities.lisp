
(in-package :guicho-utilities)
(use-syntax :annot)
(optimize*)

(declaim (inline ^2 d^2 ->rad ->deg kph->mpms mpms->kph))

@export
(defun ^2 (x) (* x x))

@export
(defun d^2 (x)
  @type double-float x
  (the double-float (* x x)))

@export
(defun ->rad (deg) (d* (d/ deg 360.0d0) +2pi+))

@export
(defun ->deg (rad) (d* (d/ rad +2pi+) 360.0d0))

@export
(defun kph->mps (kilometer-per-hour)
  (d/ (d* 1.0d3 kilometer-per-hour) 60.0d0 60.0d0))

@export
(defun mps->kph (metre-per-s)
  (d* metre-per-s 3600.0d-3))

@export
(defun kph->mpms (kilometer-per-hour)
  (d/ kilometer-per-hour 3600.0d0))

@export
(defun mpms->kph (metre-per-ms)
  (d* metre-per-ms 3600.0d0))


;; @export
;; (defun xor (a b)
;;   (and (not (and a b))
;; 	   (or a b)))

@export
(defun always-nil (&rest args)
  @ignore args
  nil)

@export
(defun always-true (&rest args)
  @ignore args
  t)

@export
(defun drandom-between (x0 x1)
  (d+ x0 (drandom (d- x1 x0))))

@export
(defun random-between (x0 x1)
  (+ x0 (random (- x1 x0))))

(setf (symbol-function 'new) #'make-instance)

@export
'new

@export
(defun collect-on-slot (s slot fn reduce-fn)
  (reduce reduce-fn
	  (map 'list fn (slot-value s slot))))

@export
(defun collect-on-reader (s reader fn reduce-fn)
  (awhen (map 'list fn (funcall reader s))
	 (reduce reduce-fn it)))


@export
(defun next-element (lst target)
  @type list lst
  (iter (for sublis on lst)
	(finding (cadr sublis) such-that (eq target (car sublis)))))

@export
(defun previous-element (lst target)
  @type list lst
  (iter (for elem in lst)
	(for prev previous elem)
	(finding prev such-that (eq target elem))))

@export
(defun nsplit-list-at (lst n)
  (iter (for i below n)
	(for sublist on lst)
	(finally
	 (let ((nthcdr (cdr sublist)))
	   (setf (cdr sublist) nil)
	   (return (list lst nthcdr))))))
					;(nsplit-list-at '(0 1 2 3 4 5 6) 3)

@export
(defun nsplit-list-from-last (lst n)
  (mapcar #'nreverse
	  (nreverse (nsplit-list-at (reverse lst) n))))

					;(nsplit-list-from-last '(0 1 2 3 4 5 6) 3)

@export
@doc "max-min &rest args -> max, min"
(defun max-min (&rest args)
  (values (apply #'max args)
	  (apply #'min args)))

@export
(defun approximate (x divisor)
  (* divisor (floor x divisor)))

@export
(defun subst-1 (new old tree &key (test #'eql))
  (let ((first t))
    (subst
     new old tree :test
     (lambda (e1 e2)
       (let ((result (and first (funcall test e1 e2))))
	 (when result
	   (setf first nil)
	   result))))))

@export
(defun nsubst-1 (new old tree &key (test #'eql))
  (let ((first t))
    (nsubst
     new old tree :test
     (lambda (e1 e2)
       (let ((result (and first (funcall test e1 e2))))
	 (when result
	   (setf first nil)
	   result))))))

;; (subst-1 2 1 '(8 8 6 (4 7 1 6) 1 9 3))


@export
@doc "traverse the tree in a depth-first manner. FN is
a function of 2 arguments, the first one is a current branch,
and the second one is the continuation funtion of 1 argument.
When you further go down the tree call that continuation with
 a certain branch.

Example:
(defun flatten (tree)
  (let ((acc nil))
    (walk-tree (lambda (branch cont)
		 (if (consp branch)
		     (funcall cont branch)
		     (push branch acc)))
	       tree)
    (nreverse acc)))"
(defun walk-tree (fn tree)
  (funcall fn tree
	   (lambda (branch)
	     (mapcar (lambda (branch)
		       (walk-tree fn branch))
		     branch))))

@export
@doc "Categorize the elements of SEQUENCE by KEY. Returns a has table. 
Each element is stored in a list (bucket) in the table."
(defun categorize (sequence &key (test #'eql) (key #'identity))
  (let ((hash (make-hash-table :test test)))
    (map nil
	 (lambda (elem)
	   (push elem (gethash (funcall key elem) hash)))
	 sequence)
    hash))