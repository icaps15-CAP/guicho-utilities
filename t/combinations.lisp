(in-package :guicho-utilities-test)
(in-suite :guicho-utilities)
(test :permutations
  (is (set-equal '((0 1 2) (1 0 2) (1 2 0) (2 1 0) (2 0 1) (0 2 1))
                 (permutations '(0 1 2))
                 :test #'equal))
  (is (set-equal '((0 1) (0 2)
                   (1 0) (1 2)
                   (2 0) (2 1))
                 (permutations '(0 1 2) :length 2)
                 :test #'equal)))

(test :combinations
  (is (equal '(())
             (combinations '(0 1 2) :length 0)))
  (is (equal '((0) (1) (2))
             (combinations '(0 1 2) :length 1)))
  (is (equal '((0 1) (0 2) (1 2))
             (combinations '(0 1 2) :length 2)))
  (is (equal '((0 1 2))
             (combinations '(0 1 2) :length 3)))
  (is (equal '((0 1 2))
             (combinations '(0 1 2)))))