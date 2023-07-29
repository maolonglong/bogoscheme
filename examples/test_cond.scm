(define (grade x)
  (cond ((>= x 90) "A")
        ((>= x 80) "B")
        ((>= x 70) "C")
        ((>= x 60) "D")
        (else "F")))

(print (grade 93))
