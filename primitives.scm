(define not
  (lambda (b)
    (if b
        #f
        #t)))

(define zero?
  (lambda (x)
    (if (= x 0)
        #t
        #f)))

(define positive?
  (lambda (x)
    (if (> x 0)
        #t
        #f)))

(define negative?
  (lambda (x)
    (if (< x 0)
        #t
        #f)))

(define min
  (lambda (x y)
    (if (< x y)
        x
        y)))

(define max
  (lambda (x y)
    (if (> x y)
        x
        y)))
