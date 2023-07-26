;; test -- computing primitive values

(define a 11)
(define b 42)
(define c 1001)
(define d (- 24)) ; unary minus

(define expra (- a (+ b c)))
(print (= expra -1032))
(define exprb (* a (/ c b)))
(print (= exprb 253))
(define exprc (* c d))
(print (= exprc -24024))
