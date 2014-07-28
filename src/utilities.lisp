
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


;; @export
;; (defun xor (a b)
;;   (and (not (and a b))
;;         (or a b)))

@export
(defun always-nil (&rest args)
  @ignore args
  (warn "obsoleted. use (cl:constantly nil)")
  nil)

@export
(defun always-true (&rest args)
  @ignore args
  (warn "obsoleted. use (cl:constantly t)")
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

@export
@doc "Categorize the elements of SEQUENCE according to the 2-arg equality function TEST.
 Returns a vector of lists. Each list contains elements which are equal in the meaning of TEST."
(defun categorize-by-equality (sequence test &key (transitive t))
  (if transitive
      (%categorize-by-equality-transitive sequence test)
      (%categorize-by-equality-intransitive sequence test)))

(defun %categorize-by-equality-transitive (sequence test)
  (reduce
   (lambda (vector element)
     (let ((i -1))
       (if (find-if (lambda (bucket)
                      (incf i)
                      (funcall test (first bucket) element))
                    vector)
           (push element (cdr (aref vector i)))
           (vector-push-extend (list element) vector)))
     vector)
   sequence :initial-value (make-array 0 :fill-pointer 0 :adjustable t))

  ;; FIXME when make-hash-table accepts functions in general!
  ;; (let ((h (make-hash-table :test test)))
  ;;   (map nil
  ;;        (lambda (elem)
  ;;          (setf (gethash elem h)
  ;;                (cons elem (gethash elem h nil))))
  ;;        sequence)
  ;;   (iter
  ;;     (with count = (hash-table-count h))
  ;;     (with vector = (make-array count :element-type '(or null (cons)) :initial-element nil))
  ;;     (for i below count)
  ;;     (for (k v) in-hashtable h)
  ;;     (setf (aref vector i) v)
  ;;     (finally (return vector))))
  )

(defun %categorize-by-equality-intransitive (sequence test)
  "Categorization based on intransitive predicate.
For example, a relation iRj s.t. i=j+-1 is intransitive because
3R4 and 4R5 holds but 3R5 doesn't hold.
However we sometimes want to categorize 3,4,5,7,8,9 by adjacency e.g.
 (3 4 5) and (7 8 9)."
  (reduce
   (lambda (acc element)
     (if-let ((buckets (remove-if-not
                        (lambda (bucket)
                          (member element bucket :test test))
                        acc)))
       (if (= 1 (length buckets))
           (let ((b (first buckets)))
             (psetf (car b) element
                    (cdr b) (cons (car b) (cdr b))))
           (progn
             ;; when two buckets shares an instance, then merge such buckets e.g.
             ;; (3 4) and (4 5) shares 4, so merge them into (3 4 5)
             (setf acc (reduce (lambda (prev bucket) (remove bucket prev))
                               buckets :initial-value acc))
             (push
              (cons element (reduce #'append buckets :from-end t))
              acc)))
       (push (list element) acc))
     acc)
   sequence :initial-value nil))

@export
(defmacro label1 (name args (&body fbody) &body body)
  `(labels ((,name ,args ,fbody)) ,@body))


@export
(defun mktemp (&optional (name "lisp") verbose)
  (iter (for path = (merge-pathnames
                     (format nil "~a.tmp.~x"
                             (string-downcase name)
                             (random MOST-POSITIVE-FIXNUM))
                     #p"/tmp/"))
        (ensure-directories-exist path :verbose verbose)
        (return-from mktemp path)))
