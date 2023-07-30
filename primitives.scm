;;;; Additional primitives implemented in Scheme.
;;;; Useful reference on what to implement as primitives:
;;;; * https://docs.racket-lang.org/r5rs/r5rs-std/index.html
;;;; * https://small.r7rs.org

;;; Begin.
;; (begin e ...) -> ((lambda () e ...))
(define-macro begin
  (lambda (exp)
    (list (cons 'lambda (cons '() (cdr exp))))))

;;; Delay and force.
;; (delay exp) -> (lambda () exp)
(define-macro delay
  (lambda (exp)
    (cons 'lambda
          (cons '()
                (cdr exp)))))

(define (force exp)
  (exp))

;;; Let.
;;; (let ((a b)) x ...) -> ((lambda (a) x ...) b)
(define-macro let
  (lambda (exp)
    ;; Be careful to define this macro using only primitives that are not
    ;; themselves defined using 'let'. Otherwise, an infinite loop may result.
    ;; Avoid the use of 'cond' because 'cond' uses 'let'.
    (if (null? (cdr exp))
      (error "Invalid let: missing variable binding list and body.")
      (if (not (list? (cadr exp)))
        (error "Invalid let: invalid variable binding list.")
        (if (null? (cddr exp))
          (error "Invalid let: missing body.")
          (cons (cons 'lambda
                      (cons
                        ;; Binding IDs.
                        (map (lambda (binding)
                               (if (not (pair? binding))
                                 (error "Invalid let: invalid variable binding list.")
                                 (if (null? (cdr binding))
                                   (error "Invalid let: every binding must have a value.")
                                   (car binding))))
                             (cadr exp))
                        ;; Body.
                        (cddr exp)))
                ;; Binding values.
                (map (lambda (binding)
                       (if (null? (cddr binding))
                         (cadr binding)
                         (error "Invalid let: invalid binding; more than one value given.")))
                     (cadr exp))))))))

;;; Cond.
;;; Expand to a chain of ifs.
(define-macro cond
  (lambda (exp)
    ;; Be careful to define this macro using only primitives that are not
    ;; themselves defined using 'cond'. Otherwise, an infinite loop may result.
    ;; In particular, avoid the use of 'cond' itself.
    (let ((clauses (cdr exp)))
      (if (null? clauses)
        (error "Invalid cond: there must be at least one clause.")
        (let ((first-clause (car clauses))
              (rest-clauses (cdr clauses)))
          (if (not (pair? first-clause))
            (error "Invalid cond: every clause must be a pair.")
            (list 'if
                  ;; Predicate.
                  ;; 'else' is only valid when it is the predicate of the last
                  ;; clause.
                  (if (and (eq? (car first-clause) 'else)
                           (null? rest-clauses))
                      #t
                      (car first-clause))
                  ;; Consequent.
                  (if (null? (cdr first-clause))
                    (error "Invalid cond: every clause must have a body.")
                    (cons 'begin (cdr first-clause)))
                  ;; Alternative.
                  (if (null? rest-clauses)
                    #f
                    (cons 'cond rest-clauses)))))))))

;;; Quasiquote, unquote, and unquote-splicing.
;;; Implementation based on "Quasiquotation in Lisp" by Alan Bawden (1999).
(define-macro quasiquote
  (lambda (exp)
    (define (qq-expand x depth)
      (cond ((pair? x)
             (cond ((eq? (car x) 'quasiquote)
                    (list 'cons
                          (list 'quote 'quasiquote)
                          (qq-expand (cdr x) (+ depth 1))))
                   ((or (eq? (car x) 'unquote)
                        (eq? (car x) 'unquote-splicing))
                    (cond ((> depth 0)
                           (list 'cons
                                 (list 'quote (car x))
                                 (qq-expand (cdr x) (- depth 1))))
                          ((and (eq? (car x) 'unquote)
                                (and (not (null? (cdr x))))
                                (null? (cddr x)))
                           (cadr x))
                          (else
                            (error "Invalid quasiquote."))))
                   (else
                     (list 'append
                           (qq-expand-list (car x) depth)
                           (qq-expand (cdr x) depth)))))
            (else
              (list 'quote x))))

    (define (qq-expand-list x depth)
      (cond ((pair? x)
             (cond ((eq? (car x) 'quasiquote)
                    (list 'list (list 'cons
                                      (list 'quote 'quasiquote)
                                      (qq-expand (cdr x) (+ depth 1)))))
                   ((or (eq? (car x) 'unquote)
                        (eq? (car x) 'unquote-splicing))
                    (cond ((> depth 0)
                           (list 'list (list 'cons
                                             (list 'quote (car x))
                                             (qq-expand (cdr x) (- depth 1)))))
                          ((eq? (car x) 'unquote)
                           (cons 'list (cdr x)))
                          (else
                            (cons 'append (cdr x)))))
                   (else
                     (list 'list (list 'append
                                       (qq-expand-list (car x) depth)
                                       (qq-expand (cdr x) depth))))))
            (else
              (list 'quote (list x)))))

    (cond ((null? (cdr exp))
           (error "Invalid quasiquote: missing argument."))
          ((null? (cddr exp))
           (qq-expand (cadr exp) 0))
          (else
            (error "Invalid quasiquote: more than one argument given.")))))

;;; Quasiquote, unquote, and unquote-splicing.
;;; Implementation based on "Quasiquotation in Lisp" by Alan Bawden (1999).
(define-macro quasiquote
  (lambda (exp)
    (define (qq-expand x depth)
      (cond ((pair? x)
             (cond ((eq? (car x) 'quasiquote)
                    (list 'cons
                          (list 'quote 'quasiquote)
                          (qq-expand (cdr x) (+ depth 1))))
                   ((or (eq? (car x) 'unquote)
                        (eq? (car x) 'unquote-splicing))
                    (cond ((> depth 0)
                           (list 'cons
                                 (list 'quote (car x))
                                 (qq-expand (cdr x) (- depth 1))))
                          ((and (eq? (car x) 'unquote)
                                (and (not (null? (cdr x))))
                                (null? (cddr x)))
                           (cadr x))
                          (else
                            (error "Invalid quasiquote."))))
                   (else
                     (list 'append
                           (qq-expand-list (car x) depth)
                           (qq-expand (cdr x) depth)))))
            (else
              (list 'quote x))))

    (define (qq-expand-list x depth)
      (cond ((pair? x)
             (cond ((eq? (car x) 'quasiquote)
                    (list 'list (list 'cons
                                      (list 'quote 'quasiquote)
                                      (qq-expand (cdr x) (+ depth 1)))))
                   ((or (eq? (car x) 'unquote)
                        (eq? (car x) 'unquote-splicing))
                    (cond ((> depth 0)
                           (list 'list (list 'cons
                                             (list 'quote (car x))
                                             (qq-expand (cdr x) (- depth 1)))))
                          ((eq? (car x) 'unquote)
                           (cons 'list (cdr x)))
                          (else
                            (cons 'append (cdr x)))))
                   (else
                     (list 'list (list 'append
                                       (qq-expand-list (car x) depth)
                                       (qq-expand (cdr x) depth))))))
            (else
              (list 'quote (list x)))))

    (cond ((null? (cdr exp))
           (error "Invalid quasiquote: missing argument."))
          ((null? (cddr exp))
           (qq-expand (cadr exp) 0))
          (else
            (error "Invalid quasiquote: more than one argument given.")))))

;;; Boolean.
;; (and) -> #t
;; (and x) -> x
;; (and x y) -> (if x y #f)
;; Takes any number of arguments.
(define-macro and
  (lambda (exp)
    (if (null? (cdr exp))  ; (and)
      #t
      (if (null? (cddr exp))  ; (and x)
        (cadr exp)
        (list 'if (cadr exp)  ; (and x0 x1 ...)
              (cons 'and (cddr exp))
              #f)))))

;; (or) -> #f
;; (or x) -> x
;; (or x y) -> (if x x y)
;; Takes any number of arguments.
(define-macro or
  (lambda (exp)
    (if (null? (cdr exp))  ; (or)
      #f
      ;; 'temp-gensymed-var is a hack. It is unhygienic.
      ;; In the future, implement syntax-rules macros or use gensym.
      (list 'let (list (list 'temp-gensymed-var (cadr exp)))  ; (or x ...)
            (list 'if 'temp-gensymed-var
                  'temp-gensymed-var
                  (cons 'or (cddr exp)))))))

(define true #t)
(define false #f)

(define (not x)
  (if x
      #f
      #t))

;;; Numbers.
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (* -1 x))))

(define (quotient n1 n2)
  (/ n1 n2))

(define (remainder n1 n2)
  (- n1 (* (/ n1 n2) n2)))

(define (zero? x)
  (= x 0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (odd? x)
  (= (remainder x 2) 1))

(define (even? x)
  (= (remainder x 2) 0))

(define (min x y)
  (if (< x y)
    x
    y))

(define (max x y)
  (if (> x y)
    x
    y))

;;; Lists.
(define null '())

(define (caar l) (car (car l)))
(define (cddr l) (cdr (cdr l)))
(define (cdar l) (cdr (car l)))
(define (cadr l) (car (cdr l)))

(define (caaar l) (car (caar l)))
(define (cdadr l) (cdr (cadr l)))
(define (cadar l) (car (cdar l)))
(define (caddr l) (car (cddr l)))
(define (caadr l) (car (cadr l)))
(define (cdaar l) (cdr (caar l)))
(define (cddar l) (cdr (cdar l)))
(define (cdddr l) (cdr (cddr l)))

(define (first l) (car l))
(define (second l) (cadr l))
(define (third l) (caddr l))
(define (rest l) (cdr l))
(define (last l) (car (reverse l)))

;; (list (+ 1 0) (+ 1 1) (+ 1 2)) -> '(1 2 3)
(define (list . xs)
  xs)

(define (length l)
  (define (loop accumulated remaining)
    (if (null? remaining)
      accumulated
      (loop (+ 1 accumulated) (cdr remaining))))
  (loop 0 l))

;; (list-ref '(a b c) 2) -> c
(define (list-ref l n)
  (cond ((null? l) (error "LIST-REF list is empty."))
        ((= n 0) (car l))
        ((< n 0) (error "LIST-REF index must be >= 0."))
        ((> n (- (length l) 1)) (error "LIST-REF index out of bounds."))
        (else (list-ref (cdr l) (- n 1)))))

(define (reverse l)
  (define (loop accumulated remaining)
    (if (null? remaining)
      accumulated
      (loop (cons (car remaining) accumulated)
            (cdr remaining))))
  (loop '() l))

(define (append . lists)
  (define (append-lol lol)
    (cond ((null? lol) '())
          ((null? (cdr lol))
           (car lol))
          (else
            (let ((first-list (car lol)))
             (if (null? first-list)
                 (append-lol (cdr lol))
                 (cons (car first-list)
                       (append-lol (cons (cdr first-list)
                                         (cdr lol)))))))))
  (append-lol lists))

(define (map f l)
  (if (null? l)
    '()
    (cons (f (car l))
          (map f (cdr l)))))

(define (filter f l)
  (cond ((null? l) '())
        ((f (car l))
         (cons (car l)
               (filter f (cdr l))))
        (else
          (filter f (cdr l)))))

;; Tail recursive. Constant space use.
(define (foldl f acc l)
  (if (null? l)
    acc
    (foldl f
           (f acc (car l))
           (cdr l))))

;; Not tail recursive. Space use is proportional to length of list.
(define (foldr f acc l)
  (if (null? l)
    acc
    (f (car l)
       (foldr f acc (cdr l)))))

(define (for-each f l)
  (cond ((null? l) '())
         (else
           (f (car l))
           (for-each f (cdr l)))))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
        (else (member x (cdr l)))))

(define (index-of l x)
  (define (loop l x acc)
    (cond ((null? l) #f)
          ((equal? (car l) x)
           acc)
          (else
           (loop (cdr l) x (+ acc 1)))))
  (loop l x 0))

;; (assoc 2 '((1 a) (2 b))) -> '(2 b)
;; (assoc 2 '((1 a))) -> #f
(define (assoc x al)
  (cond ((null? al) #f)
        ((equal? x (caar al)) (car al))
        (else (assoc x (cdr al)))))

;; (remove 2 '(1 2 3 2 1)) -> '(1 3 1)
(define (remove x l)
  (filter (lambda (e) (not (equal? e x)))
          l))

;; (take '(1 2 3 4) 2) -> '(1 2)
(define (take l n)
  (cond ((= n 0) '())
        (else
          (cons (car l)
                (take (cdr l) (- n 1))))))

;; (drop '(1 2 3 4) 2) -> '(3 4)
(define (drop l n)
  (cond ((= n 0) l)
        (else
          (drop (cdr l) (- n 1)))))
(define list-tail drop)  ; R5RS procedure.

;;; Display.
(define (newline)
  (display "\n"))

(define (displayln s)
  (display s)
  (newline))
