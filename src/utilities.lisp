
(in-package :guicho-utilities)
(use-syntax :annot)
(optimize*)

(declaim (inline ^2 d^2 ->rad ->deg kph->mpms mpms->kph))

@export
(defun density-char (n)
  (case n
    (0 #\Space)
    (1 #\.)
    (2 #\,)
    (3 #\:)
    (4 #\;)
    (5 #\i)
    (6 #\j)
    (7 #\!)
    (8 #\|)
    (9 #\l)
    (10 #\I)
    (11 #\k)
    (12 #\K)
    (13 #\M)
    (14 #\W)
    (15 #\#)
    (16 #\0)
    (17 #\@)
    (t  #\■)))

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
(defmacro label1 (name args (&body fbody) &body body)
  `(labels ((,name ,args ,fbody)) ,@body))

(defun %query-path (name)
  (let ((path (merge-pathnames
               #+sbcl
               (format nil "lisptmp/~a.~a.~x"
                       (string-downcase name)
                       (sb-posix:getpid)
                       (random MOST-POSITIVE-FIXNUM))
               #-sbcl
               (format nil "lisptmp/~a.~x"
                       (string-downcase name)
                       (random MOST-POSITIVE-FIXNUM))
               #p"/tmp/")))
    (if (probe-file path) nil path)))



@export
(defun mktemp (&optional (name "lisp") verbose)
  (iter (for path = (%query-path name))
        (when path
          (ensure-directories-exist path :verbose verbose)
          (return-from mktemp path))))
