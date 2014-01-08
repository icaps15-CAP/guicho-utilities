
(in-package :guicho-utilities)
(use-syntax :annot)

@export
(defun permutations (seq)
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
    (cons (%permutations-of-cons seq))))

(defun %permutations-of-cons (seq)
  (match seq
    ((list _)
     (list seq))
    ((list* head rest-seq)
     (let* ((len (length seq))
            (n (1- len))) ; n -- 2, length -- 3
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
         (forward (%permutations-of-cons rest-seq) 0 nil))))))


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

;; http://stackoverflow.com/questions/7292370/recursive-combinations

;; build_combinations (tokens, set_size)
;;   Arrangements combos
;;   if (set_size == 0)
;;     combos.add ("")
;;   else
;;     Comment: tail_substrings of "ABC" is ("ABC", "BC", "C").
;;     foreach tail (tail_substrings (tokens))
;;       foreach sub_combo (build_combinations (tail, set_size-1))
;;         combos.add (tail.first() + sub_combo)
;;   return combos


@export
(defun combinations (seq &key (length (length seq)))
  "returns a list of sequences.
   each sequence is a k-combination of the original sequence."
  (etypecase seq
    (cons (%combinations-of-cons seq length))))

(defun %combinations-of-cons (seq length)
  (if (zerop length) ;;(= length (length seq))
      (list (list))
      (%per-tail length seq)))

(defun %per-tail (length tail)
  (if tail
      (%attach-head length tail (car tail) (%combinations-of-cons tail (1- length)))
      (list)))

(defun %attach-head (length tail head sub-combo)
  (if sub-combo
      (cons (cons head (car sub-combo))
            (%attach-head length tail head (cdr sub-combo)))
      (%per-tail length (cdr tail))))
