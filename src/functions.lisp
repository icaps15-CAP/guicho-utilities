(in-package :guicho-utilities)
(use-syntax :annot)

;; available in alexandria
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
@doc "Categorize the elements of SEQUENCE according to the 2-arg
equality function TEST.  Returns a vector of lists. Each list contains
elements which are equal in the meaning of TEST.  When the
keyword :transitive is non-nil, it assumes the test function is transitive,
and it uses the algorithm that can benefit from the lazy evaluation
in the test function."
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
