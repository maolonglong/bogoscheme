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

(define (map f l)
  (if (null? l)
    '()
    (cons (f (car l))
          (map f (cdr l)))))

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
