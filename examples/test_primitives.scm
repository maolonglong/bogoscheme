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

;;;; Unit tests for primitives.scm.
(load "lib-assert.scm")

;;; Quasiquote.
(assert-equal `x (quasiquote x))
(assert-equal `x 'x)
(assert-equal `(0 1 2) (quasiquote (0 1 2)))
(assert-equal `(0 1 2) '(0 1 2))
(assert-equal `(0 1 ,(+ 1 1) 3) (quasiquote (0 1 (unquote (+ 1 1)) 3)))
(assert-equal `(0 1 ,(+ 1 1) 3) '(0 1 2 3))
;; Quasiquoted improper list.
(assert-equal `(1 . 2) '(1 . 2))
(assert-equal `(1 . ,(+ 1 1)) '(1 . 2))
;; Quasiquotes within quasiquotes.
(assert-equal ``,(+ 1 2) '(quasiquote (unquote (+ 1 2))))
(assert-equal ``,,(+ 1 2) '(quasiquote (unquote 3)))
(assert-equal `,`,(+ 1 2) 3)
(assert-equal ```,,,(+ 1 2) '(quasiquote (quasiquote (unquote (unquote 3)))))
(assert-equal ``,`,,(+ 1 2) '(quasiquote (unquote (quasiquote (unquote 3)))))

;; FIXME: it runs very slowly???
(assert-equal `(1 `(2 ,(3 ,(+ 4 5) 6) 7) 8)
              '(1 (quasiquote (2 (unquote (3 9 6)) 7)) 8))

;; Unquote-splicing.
(assert-equal `(,@(list 1 2 3)) '(1 2 3))
(assert-equal `(1 ,@(list 2 3) 4) '(1 2 3 4))
(assert-equal `(1 `,,@(list (+ 1 1)) 3) '(1 (quasiquote (unquote 2)) 3))

(assert-true (and))
(assert-equal (and 'x) 'x)
(assert-equal (and #t 'x) 'x)
(assert-equal (and 'x #t) #t)
(assert-false (and #f #t #t #t))
(assert-false (and 'x #f #t #t))
(assert-equal (and 'a 'b 'c 'd) 'd)

(assert-false (or))
(assert-equal (or 'x) 'x)
(assert-equal (or #f 'x) 'x)
(assert-equal (or 'x #f) 'x)
(assert-equal (or #f #f #t #f) #t)
(assert-equal (or #f #f 'x #f) 'x)
(assert-equal (or 'a 'b 'c 'd) 'a)

(assert-true (>= 3 3))
(assert-true (>= 10 1))
(assert-true (>= -1 -10))
(assert-false (>= -1 0))
(assert-false (>= -10 -1))

(assert-equal (abs 1) 1)
(assert-equal (abs 0) 0)
(assert-equal (abs -1) 1)

(assert-equal (quotient 9 3) 3)
(assert-equal (quotient 9 -3) -3)
(assert-equal (quotient -9 3) -3)
(assert-equal (quotient -9 -3) 3)

(assert-equal (remainder 4 2) 0)
(assert-equal (remainder -4 2) 0)
(assert-equal (remainder -4 -2) 0)
(assert-equal (remainder 5 2) 1)
(assert-equal (remainder 5 -2) 1)
(assert-equal (remainder -5 2) -1)
(assert-equal (remainder -5 -2) -1)

(assert-true (zero? 0))
(assert-false (zero? 1))
(assert-false (zero? -1))

(assert-true (positive? 1))
(assert-false (positive? 0))
(assert-false (positive? -1))

(assert-true (negative? -1))
(assert-false (negative? 0))
(assert-false (negative? 1))

(assert-true (odd? 3))
(assert-false (odd? 2))

(assert-true (even? 0))
(assert-true (even? 2))
(assert-false (even? 1))

(assert-equal (min 0 0) 0)
(assert-equal (min -1 -1) -1)
(assert-equal (min -1 1) -1)
(assert-equal (min 1 -1) -1)
(assert-equal (min 1 2) 1)

(assert-equal (max 0 0) 0)
(assert-equal (max -1 -1) -1)
(assert-equal (max -1 1) 1)
(assert-equal (max 1 -1) 1)
(assert-equal (max 1 2) 2)

(define l1 '())
(define l2 '(1 2 3 4 5 6))
(define l3 '((a b c) (1 2 3) ("do" "re" "mi")))
(assert-equal (caar l3) 'a)

; Missing: cddr, cadr, first, second, etc.

(assert-equal (list (+ 1 0) (+ 1 1) (+ 1 2)) '(1 2 3))

(assert-equal (length l1) 0)
(assert-equal (length l2) 6)

(assert-equal (list-ref l2 0) 1)
(assert-equal (list-ref l2 5) 6)

(assert-equal (reverse '()) '())
(assert-equal (reverse '(1 2 3)) '(3 2 1))

(assert-equal (append '() '()) '())
(assert-equal (append '(1 2 3) '()) '(1 2 3))
(assert-equal (append '() '(4 5 6)) '(4 5 6))
(assert-equal (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(assert-equal (append '(1 2) '(3 4) '(5 6)) '(1 2 3 4 5 6))
(assert-equal (append '(1 2) '() '(3 4) '() '(5 6)) '(1 2 3 4 5 6))
(assert-equal (append 'a) 'a)
(assert-equal (append '(a) 'b) '(a . b))
(assert-equal (append '(a b) 'c) '(a b . c))
(assert-equal (append '(a b) '(c . d)) '(a b c . d))

(assert-equal (map (lambda (x) x) '()) '())
(assert-equal (map (lambda (x) (+ x 1)) '(1 2)) '(2 3))

(assert-equal (filter (lambda (x) 'lala) '()) '())
(assert-equal (filter (lambda (x) #t) '(1 2 3)) '(1 2 3))
(assert-equal (filter even? '(1 2 3 4 5 6)) '(2 4 6))

(assert-equal (foldl (lambda (acc x) (+ acc x)) 0 '(1 2 3)) 6)
(assert-equal (foldr (lambda (x acc) (+ acc x)) 0 '(1 2 3)) 6)

; Missing: for-each.

(assert-true (member 6 l2))
(assert-false (member 999 l2))

(assert-equal (index-of l2 1) 0)
(assert-equal (index-of l2 6) 5)
(assert-false (index-of l2 999))

(assert-equal (assoc 2 '((1 a) (2 b))) '(2 b))
(assert-false (assoc 3 '((1 a) (2 b))))

(assert-equal (remove 'z '()) '())
(assert-equal (remove 'z '(a b c)) '(a b c))
(assert-equal (remove 'z '(z z z)) '())
(assert-equal (remove 'z '(z zoo z zoom)) '(zoo zoom))

(define l '(1 2 3))
(assert-equal (take l 0) '())
(assert-equal (take l 1) '(1))
(assert-equal (take l 3) l)

(assert-equal (drop l 0) l)
(assert-equal (drop l 1) '(2 3))
(assert-equal (drop l 3) '())
