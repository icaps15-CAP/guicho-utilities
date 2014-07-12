(in-package :guicho-utilities-test)
(in-suite :guicho-utilities)
(test :with-iter-array
  (let ((a #2A((0 0 0 1 0)
               (0 1 0 1 0)
               (1 1 1 0 0)))
        results)
    (with-iter-array (n i j) a
      (when (plusp n)
        (push (list i j) results)))
    (is (set-equal '((0 3) (1 1) (1 3) (2 0) (2 1) (2 2))
                   results
                   :test #'equal))))

(test :with-iter-array-row-major
  (let ((a #2A((0 0 0 1 0)
               (0 1 0 1 0)
               (1 1 1 0 0)))
        results)
    (with-iter-array-row-major (n i) a
      (when (plusp n)
        (push i results)))
    (is (set-equal '(3 6 8 10 11 12)
                   results))))
