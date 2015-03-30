(in-package :guicho-utilities-test)
(in-suite :guicho-utilities)

(def-suite :more-labels :in :guicho-utilities)
(in-suite :more-labels)

(test local-function
  (finishes
    (define-local-function lfn1 (x y)
      (+ x y)))
  (finishes
    (define-local-function lfn2 (x y)
      (return-from lfn2
        (* x y))))
  (finishes
    (define-local-function lfn3 (x y)
      (+ (lfn2 x y) (lfn1 x y)))))

(test (more-labels :depends-on local-function)
  (is (= 7
         (more-labels () (lfn1)
           (lfn1 2 5)))))

(test (return-from :depends-on local-function)
  ;; ensure `return-from' works correctly
  (is (= 10
         (more-labels () (lfn2)
           (lfn2 2 5)))))

(test (interaction :depends-on local-function)
  ;; lfn3 uses lfn1 and lfn2
  ;; test both inlined and not-inlined version
  (is (= 17
         (more-labels () (lfn1 lfn2 lfn3)
           (lfn3 2 5)))))

(test (overwrite :depends-on local-function)
  ;; ensure overwriting `lfn2' does not affect the local definition
  (defun lfn2 (x y)
    (/ x y))
  (is (= 10
         (more-labels () (lfn2)
           (lfn2 2 5)))))
