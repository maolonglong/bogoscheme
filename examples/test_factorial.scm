;; test -- computing factorials

(define number 10)

(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

(print (= (factorial number) 3628800))
