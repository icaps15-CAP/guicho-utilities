
(in-package :guicho-utilities)
(use-syntax :annot)

@export
(defun permutations (seq &key length)
  "returns a list of sequences.
   each sequence is a permutation of the original sequence.

Based on Steinhaus–Johnson–Trotter algorithm + Even's speedup

;; 1

;; 2 1
;; 1 2

;; 3 2 1  -- forward
;; 2 3 1
;; 2 1 3
;; 1 2 3  -- backward
;; 1 3 2
;; 3 1 2

"
  (etypecase seq
    (cons (let ((seq-size (length seq)))
            (if (or (null length)
                    (= length seq-size))
                (%permutations-of-cons seq seq-size seq-size)
                (mappend (lambda (subseq)
                           (%permutations-of-cons subseq length length))
                         (%combinations-of-cons seq seq-size length)))))))

(defun %permutations-of-cons (seq seq-size length)
  (match seq
    ((list _)
     (list seq))
    ((list* head rest-seq)
     (let* ((n (1- length))) ; n -- 2, length -- 3
       (labels 
           ((insert (subperm pos)
              (let (acc (%subperm subperm))
                (dotimes (i pos)        ; i = 0
                  (push (car %subperm) acc)
                  (setf %subperm (cdr %subperm)))
                (push head acc); pos = 1
                (dotimes (i (- n pos))  ; 2 - 1 = 1
                  (push (car %subperm) acc)
                  (setf %subperm (cdr %subperm)))
                (nreverse acc)))
                                        ;
            (forward (subperms pos acc) ; pos -- 0,1,2
              (ematch subperms
                ((cons subperm rest)
                 (if (< pos n)
                     (forward subperms (1+ pos)
                              (cons (insert subperm pos) acc))
                     (backward rest pos
                               (cons (insert subperm pos) acc))))
                (_ acc)))
            (backward (subperms pos acc)
              (ematch subperms
                ((cons subperm rest)
                 (if (< 0 pos)
                     (backward subperms (1- pos)
                               (cons (insert subperm pos) acc))
                     (forward rest pos
                              (cons (insert subperm pos) acc))))
                (_ acc))))
         (forward (%permutations-of-cons rest-seq (1- seq-size) n) 0 nil))))))

;; non-tail-recursive version

;; (defun %permutations-of-cons (seq length)
;;   (match seq
;;     ((list _)
;;      (llist seq))
;;     ((list* head rest-seq)
;;      (let* ((n (1- length))) ; n -- 2, length -- 3
;;        (labels 
;;            ((insert (subperm pos)
;;               (let (acc (%subperm subperm))
;;                 (dotimes (i pos)        ; i = 0
;;                   (push (car %subperm) acc)
;;                   (setf %subperm (cdr %subperm)))
;;                 (push head acc); pos = 1
;;                 (dotimes (i (- n pos))  ; 2 - 1 = 1
;;                   (push (car %subperm) acc)
;;                   (setf %subperm (cdr %subperm)))
;;                 (nreverse acc)))
;;             (forward (subperms pos)
;;               (match subperms
;;                 ((cons subperm rest)
;;                  (if (< pos n)
;;                      (cons (insert subperm pos) (forward subperms (1+ pos)))
;;                      (cons (insert subperm pos) (backward rest pos))))))
;;             (backward (subperms pos)
;;               (match subperms
;;                 ((cons subperm rest)
;;                  (if (< 0 pos)
;;                      (cons (insert subperm pos) (backward subperms (1- pos)))
;;                      (cons (insert subperm pos) (forward rest pos)))))))
;;          (forward (%lpermutations-of-cons rest-seq n) 0))))))

;; less efficient, bad performance in both memory and speed
;; 
;; @export
;; (defun permutations (lst)
;;   (if (cdr lst)
;;       (iter appender
;;             (for after on lst)
;;             (for e = (car after))
;;             (for rest = (append before (cdr after)))
;;             (iter
;;               (for perm in (permutations-of rest))
;;               (in appender (collect (cons e perm))))
;;             (collect e into before))
;;       (list lst)))
;; 

;; combinations, implemented in a functional style

@export
(defun combinations (seq &key (length (length seq)))
  "returns a list of sequences.
   each sequence is a k-combination of the original sequence."
  (etypecase seq
    (cons (%combinations-of-cons seq (length seq) length))))

;; non-tail-recursive

(defun %combinations-of-cons (seq seq-size length)
  (multiple-value-match (values length seq)
    ((0 _)   (list (list)))
    ((_ nil) (list (list)))
    ((k (cons x xs))
     (let ((xs-size (1- seq-size)))
       (if (<= k xs-size)
           (append (mapcar (lambda (rest)
                             (cons x rest))
                           (%combinations-of-cons xs xs-size (1- k)))
                   (%combinations-of-cons xs xs-size k))
           (mapcar (lambda (rest)
                     (cons x rest))
                   (%combinations-of-cons xs xs-size (1- k))))))))

