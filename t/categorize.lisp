(in-package :guicho-utilities-test)
(in-suite :guicho-utilities)
(test :categorize-by-equality
  (let (cat)
    (finishes
      (setf cat (categorize-by-equality
                 (iota 10)
                 (lambda (a b)
                   (= (mod a 3)
                      (mod b 3))))))
    (print cat)
    (is (= 3 (length cat))))

  ;; intransitive
  (flet ((next-to (a b) (or (= (1+ a) b) (= (1+ b) a))))
    (let (cat)
      (finishes
        (setf cat (categorize-by-equality
                   (shuffle (append (iota 10) (iota 10 :start 20)))
                   #'next-to)))
      (print '(:transitive t))
      (print cat)
      (is-false (= 2 (length cat)))

      (finishes
        (setf cat (categorize-by-equality
                   (shuffle (append (iota 10) (iota 10 :start 20)))
                   #'next-to
                   :transitive nil)))
      (print '(:transitive nil))
      (print cat)
      (is-true (= 2 (length cat))))))
