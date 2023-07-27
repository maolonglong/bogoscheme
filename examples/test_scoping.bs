;; test -- lexical scoping

(define makeadder
  (lambda (n)
    (lambda (m)
      (+ m n))))

(define addthree (makeadder 3))
(print (= (addthree 3) 6))

(define scopetester
  (lambda (n)
    (print (= ((lambda (n) (+ n 12)) 24) 36))
    (print (= n 101))))

(scopetester 101)
