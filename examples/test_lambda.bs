;; test -- lambda expressions with multiple subexpressions

(define f
  (lambda (x y)
    (print #t)
    (+ x y)))

(print (= (f 2 3) 5))
