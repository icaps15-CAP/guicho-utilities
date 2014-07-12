(in-package :guicho-utilities-test)
(in-suite :guicho-utilities)

(def-suite :more-labels :in :guicho-utilities)
(in-suite :more-labels)

(test local-function
  (finishes
    (define-local-function fn1 (x y)
      (+ x y)))
  (finishes
    (define-local-function fn2 (x y)
      (return-from fn2
        (* x y))))
  (finishes
    (define-local-function fn3 (x y)
      (+ (fn2 x y) (fn1 x y)))))

(test (more-labels :depends-on local-function)
  (is (= 7
         (more-labels () (fn1)
           (fn1 2 5)))))

(test (return-from :depends-on local-function)
  ;; ensure `return-from' works correctly
  (is (= 10
         (more-labels () (fn2)
           (fn2 2 5)))))

(test (interaction :depends-on local-function)
  ;; fn3 uses fn1 and fn2
  ;; test both inlined and not-inlined version
  (is (= 17
         (more-labels () (fn1 fn2 fn3)
           (fn3 2 5)))))

(test (overwrite :depends-on local-function)
  ;; ensure overwriting `fn2' does not affect the local definition
  (defun fn2 (x y)
    (/ x y))
  (is (= 10
         (more-labels () (fn2)
           (fn2 2 5)))))
