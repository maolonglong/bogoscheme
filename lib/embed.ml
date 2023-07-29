let primitives_dot_scm =
  ";;; Begin.\n\
   ;; (begin e ...) -> ((lambda () e ...))\n\
   (define-macro begin\n\
  \  (lambda (exp)\n\
  \    (list (cons 'lambda (cons '() (cdr exp))))))\n\n\
   ;;; Delay and force.\n\
   ;; (delay exp) -> (lambda () exp)\n\
   (define-macro delay\n\
  \  (lambda (exp)\n\
  \    (cons 'lambda\n\
  \          (cons '()\n\
  \                (cdr exp)))))\n\n\
   (define (force exp)\n\
  \  (exp))\n\n\
   ;;; Let.\n\
   ;;; (let ((a b)) x ...) -> ((lambda (a) x ...) b)\n\
   (define-macro let\n\
  \  (lambda (exp)\n\
  \    ;; Be careful to define this macro using only primitives that are not\n\
  \    ;; themselves defined using 'let'. Otherwise, an infinite loop may result.\n\
  \    ;; Avoid the use of 'cond' because 'cond' uses 'let'.\n\
  \    (if (null? (cdr exp))\n\
  \      (error \"Invalid let: missing variable binding list and body.\")\n\
  \      (if (not (list? (cadr exp)))\n\
  \        (error \"Invalid let: invalid variable binding list.\")\n\
  \        (if (null? (cddr exp))\n\
  \          (error \"Invalid let: missing body.\")\n\
  \          (cons (cons 'lambda\n\
  \                      (cons\n\
  \                        ;; Binding IDs.\n\
  \                        (map (lambda (binding)\n\
  \                               (if (not (pair? binding))\n\
  \                                 (error \"Invalid let: invalid variable binding \
   list.\")\n\
  \                                 (if (null? (cdr binding))\n\
  \                                   (error \"Invalid let: every binding must have a \
   value.\")\n\
  \                                   (car binding))))\n\
  \                             (cadr exp))\n\
  \                        ;; Body.\n\
  \                        (cddr exp)))\n\
  \                ;; Binding values.\n\
  \                (map (lambda (binding)\n\
  \                       (if (null? (cddr binding))\n\
  \                         (cadr binding)\n\
  \                         (error \"Invalid let: invalid binding; more than one value \
   given.\")))\n\
  \                     (cadr exp))))))))\n\n\
   ;;; Boolean.\n\
   ;; (and) -> #t\n\
   ;; (and x) -> x\n\
   ;; (and x y) -> (if x y #f)\n\
   ;; Takes any number of arguments.\n\
   (define-macro and\n\
  \  (lambda (exp)\n\
  \    (if (null? (cdr exp))  ; (and)\n\
  \      #t\n\
  \      (if (null? (cddr exp))  ; (and x)\n\
  \        (cadr exp)\n\
  \        (list 'if (cadr exp)  ; (and x0 x1 ...)\n\
  \              (cons 'and (cddr exp))\n\
  \              #f)))))\n\n\
   ;; (or) -> #f\n\
   ;; (or x) -> x\n\
   ;; (or x y) -> (if x x y)\n\
   ;; Takes any number of arguments.\n\
   (define-macro or\n\
  \  (lambda (exp)\n\
  \    (if (null? (cdr exp))  ; (or)\n\
  \      #f\n\
  \      ;; 'temp-gensymed-var is a hack. It is unhygienic.\n\
  \      ;; In the future, implement syntax-rules macros or use gensym.\n\
  \      (list 'let (list (list 'temp-gensymed-var (cadr exp)))  ; (or x ...)\n\
  \            (list 'if 'temp-gensymed-var\n\
  \                  'temp-gensymed-var\n\
  \                  (cons 'or (cddr exp)))))))\n\n\
   (define not\n\
  \  (lambda (b)\n\
  \    (if b\n\
  \        #f\n\
  \        #t)))\n\n\
   (define zero?\n\
  \  (lambda (x)\n\
  \    (if (= x 0)\n\
  \        #t\n\
  \        #f)))\n\n\
   (define positive?\n\
  \  (lambda (x)\n\
  \    (if (> x 0)\n\
  \        #t\n\
  \        #f)))\n\n\
   (define negative?\n\
  \  (lambda (x)\n\
  \    (if (< x 0)\n\
  \        #t\n\
  \        #f)))\n\n\
   (define min\n\
  \  (lambda (x y)\n\
  \    (if (< x y)\n\
  \        x\n\
  \        y)))\n\n\
   (define max\n\
  \  (lambda (x y)\n\
  \    (if (> x y)\n\
  \        x\n\
  \        y)))\n\n\
   ;;; Lists.\n\
   (define null '())\n\n\
   (define (caar l) (car (car l)))\n\
   (define (cddr l) (cdr (cdr l)))\n\
   (define (cdar l) (cdr (car l)))\n\
   (define (cadr l) (car (cdr l)))\n\n\
   (define (caaar l) (car (caar l)))\n\
   (define (cdadr l) (cdr (cadr l)))\n\
   (define (cadar l) (car (cdar l)))\n\
   (define (caddr l) (car (cddr l)))\n\
   (define (caadr l) (car (cadr l)))\n\
   (define (cdaar l) (cdr (caar l)))\n\
   (define (cddar l) (cdr (cdar l)))\n\
   (define (cdddr l) (cdr (cddr l)))\n\n\
   (define (first l) (car l))\n\
   (define (second l) (cadr l))\n\
   (define (third l) (caddr l))\n\
   (define (rest l) (cdr l))\n\
   (define (last l) (car (reverse l)))\n\n\
   ;; (list (+ 1 0) (+ 1 1) (+ 1 2)) -> '(1 2 3)\n\
   (define (list . xs)\n\
  \  xs)\n\n\
   (define (map f l)\n\
  \  (if (null? l)\n\
  \    '()\n\
  \    (cons (f (car l))\n\
  \          (map f (cdr l)))))\n\n\
   ;;; Cond.\n\
   ;;; Expand to a chain of ifs.\n\
   (define-macro cond\n\
  \  (lambda (exp)\n\
  \    ;; Be careful to define this macro using only primitives that are not\n\
  \    ;; themselves defined using 'cond'. Otherwise, an infinite loop may result.\n\
  \    ;; In particular, avoid the use of 'cond' itself.\n\
  \    (let ((clauses (cdr exp)))\n\
  \      (if (null? clauses)\n\
  \        (error \"Invalid cond: there must be at least one clause.\")\n\
  \        (let ((first-clause (car clauses))\n\
  \              (rest-clauses (cdr clauses)))\n\
  \          (if (not (pair? first-clause))\n\
  \            (error \"Invalid cond: every clause must be a pair.\")\n\
  \            (list 'if\n\
  \                  ;; Predicate.\n\
  \                  ;; 'else' is only valid when it is the predicate of the last\n\
  \                  ;; clause.\n\
  \                  (if (and (eq? (car first-clause) 'else)\n\
  \                           (null? rest-clauses))\n\
  \                      #t\n\
  \                      (car first-clause))\n\
  \                  ;; Consequent.\n\
  \                  (if (null? (cdr first-clause))\n\
  \                    (error \"Invalid cond: every clause must have a body.\")\n\
  \                    (cons 'begin (cdr first-clause)))\n\
  \                  ;; Alternative.\n\
  \                  (if (null? rest-clauses)\n\
  \                    #f\n\
  \                    (cons 'cond rest-clauses)))))))))\n"
;;
