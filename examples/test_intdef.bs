;; test -- internal defines

(define foo
  (lambda (x)
    (define bar
      (lambda (y) (+ x y)))
    (+ x (bar (* 3 x)))))

(print (= (foo 12) 60))
