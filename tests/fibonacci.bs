(define fib-tailrec-aux
  (lambda (n x y)
    (if (= n 0)
        x
        (fib-tailrec-aux (- n 1) y (+ x y)))))

(define fib-tailrec
  (lambda (n)
    (fib-tailrec-aux n 0 1)))

(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 2)) (fib (- n 1))))))

(print (fib-tailrec 0))
(print (fib-tailrec 1))
(print (fib-tailrec 2))
(print (fib-tailrec 3))
(print (fib-tailrec 4))
(print (fib-tailrec 5))
(print (fib-tailrec 10))
(print (fib-tailrec 35))
(print (fib-tailrec 50))

;; slow !!!
; (print (fib 35))
