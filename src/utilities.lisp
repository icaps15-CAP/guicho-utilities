
(in-package :guicho-utilities)
(annot:enable-annot-syntax)
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
