;; test -- multi-argument functions

(define f
  (lambda (x y z)
    (+ x (* y z))))

(print (= (f 2 3 4) 14))
