let primitives_dot_scm =
  ";;;; Additional primitives implemented in Scheme.\n\
   ;;;; Useful reference on what to implement as primitives:\n\
   ;;;; * https://docs.racket-lang.org/r5rs/r5rs-std/index.html\n\
   ;;;; * https://small.r7rs.org\n\n\
   ;;; Begin.\n\
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
  \                    (cons 'cond rest-clauses)))))))))\n\n\
   ;;; Quasiquote, unquote, and unquote-splicing.\n\
   ;;; Implementation based on \"Quasiquotation in Lisp\" by Alan Bawden (1999).\n\
   (define-macro quasiquote\n\
  \  (lambda (exp)\n\
  \    (define (qq-expand x depth)\n\
  \      (cond ((pair? x)\n\
  \             (cond ((eq? (car x) 'quasiquote)\n\
  \                    (list 'cons\n\
  \                          (list 'quote 'quasiquote)\n\
  \                          (qq-expand (cdr x) (+ depth 1))))\n\
  \                   ((or (eq? (car x) 'unquote)\n\
  \                        (eq? (car x) 'unquote-splicing))\n\
  \                    (cond ((> depth 0)\n\
  \                           (list 'cons\n\
  \                                 (list 'quote (car x))\n\
  \                                 (qq-expand (cdr x) (- depth 1))))\n\
  \                          ((and (eq? (car x) 'unquote)\n\
  \                                (and (not (null? (cdr x))))\n\
  \                                (null? (cddr x)))\n\
  \                           (cadr x))\n\
  \                          (else\n\
  \                            (error \"Invalid quasiquote.\"))))\n\
  \                   (else\n\
  \                     (list 'append\n\
  \                           (qq-expand-list (car x) depth)\n\
  \                           (qq-expand (cdr x) depth)))))\n\
  \            (else\n\
  \              (list 'quote x))))\n\n\
  \    (define (qq-expand-list x depth)\n\
  \      (cond ((pair? x)\n\
  \             (cond ((eq? (car x) 'quasiquote)\n\
  \                    (list 'list (list 'cons\n\
  \                                      (list 'quote 'quasiquote)\n\
  \                                      (qq-expand (cdr x) (+ depth 1)))))\n\
  \                   ((or (eq? (car x) 'unquote)\n\
  \                        (eq? (car x) 'unquote-splicing))\n\
  \                    (cond ((> depth 0)\n\
  \                           (list 'list (list 'cons\n\
  \                                             (list 'quote (car x))\n\
  \                                             (qq-expand (cdr x) (- depth 1)))))\n\
  \                          ((eq? (car x) 'unquote)\n\
  \                           (cons 'list (cdr x)))\n\
  \                          (else\n\
  \                            (cons 'append (cdr x)))))\n\
  \                   (else\n\
  \                     (list 'list (list 'append\n\
  \                                       (qq-expand-list (car x) depth)\n\
  \                                       (qq-expand (cdr x) depth))))))\n\
  \            (else\n\
  \              (list 'quote (list x)))))\n\n\
  \    (cond ((null? (cdr exp))\n\
  \           (error \"Invalid quasiquote: missing argument.\"))\n\
  \          ((null? (cddr exp))\n\
  \           (qq-expand (cadr exp) 0))\n\
  \          (else\n\
  \            (error \"Invalid quasiquote: more than one argument given.\")))))\n\n\
   ;;; Quasiquote, unquote, and unquote-splicing.\n\
   ;;; Implementation based on \"Quasiquotation in Lisp\" by Alan Bawden (1999).\n\
   (define-macro quasiquote\n\
  \  (lambda (exp)\n\
  \    (define (qq-expand x depth)\n\
  \      (cond ((pair? x)\n\
  \             (cond ((eq? (car x) 'quasiquote)\n\
  \                    (list 'cons\n\
  \                          (list 'quote 'quasiquote)\n\
  \                          (qq-expand (cdr x) (+ depth 1))))\n\
  \                   ((or (eq? (car x) 'unquote)\n\
  \                        (eq? (car x) 'unquote-splicing))\n\
  \                    (cond ((> depth 0)\n\
  \                           (list 'cons\n\
  \                                 (list 'quote (car x))\n\
  \                                 (qq-expand (cdr x) (- depth 1))))\n\
  \                          ((and (eq? (car x) 'unquote)\n\
  \                                (and (not (null? (cdr x))))\n\
  \                                (null? (cddr x)))\n\
  \                           (cadr x))\n\
  \                          (else\n\
  \                            (error \"Invalid quasiquote.\"))))\n\
  \                   (else\n\
  \                     (list 'append\n\
  \                           (qq-expand-list (car x) depth)\n\
  \                           (qq-expand (cdr x) depth)))))\n\
  \            (else\n\
  \              (list 'quote x))))\n\n\
  \    (define (qq-expand-list x depth)\n\
  \      (cond ((pair? x)\n\
  \             (cond ((eq? (car x) 'quasiquote)\n\
  \                    (list 'list (list 'cons\n\
  \                                      (list 'quote 'quasiquote)\n\
  \                                      (qq-expand (cdr x) (+ depth 1)))))\n\
  \                   ((or (eq? (car x) 'unquote)\n\
  \                        (eq? (car x) 'unquote-splicing))\n\
  \                    (cond ((> depth 0)\n\
  \                           (list 'list (list 'cons\n\
  \                                             (list 'quote (car x))\n\
  \                                             (qq-expand (cdr x) (- depth 1)))))\n\
  \                          ((eq? (car x) 'unquote)\n\
  \                           (cons 'list (cdr x)))\n\
  \                          (else\n\
  \                            (cons 'append (cdr x)))))\n\
  \                   (else\n\
  \                     (list 'list (list 'append\n\
  \                                       (qq-expand-list (car x) depth)\n\
  \                                       (qq-expand (cdr x) depth))))))\n\
  \            (else\n\
  \              (list 'quote (list x)))))\n\n\
  \    (cond ((null? (cdr exp))\n\
  \           (error \"Invalid quasiquote: missing argument.\"))\n\
  \          ((null? (cddr exp))\n\
  \           (qq-expand (cadr exp) 0))\n\
  \          (else\n\
  \            (error \"Invalid quasiquote: more than one argument given.\")))))\n\n\
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
   (define true #t)\n\
   (define false #f)\n\n\
   (define (not x)\n\
  \  (if x\n\
  \      #f\n\
  \      #t))\n\n\
   ;;; Numbers.\n\
   (define (abs x)\n\
  \  (cond ((> x 0) x)\n\
  \        ((= x 0) 0)\n\
  \        ((< x 0) (* -1 x))))\n\n\
   (define (quotient n1 n2)\n\
  \  (/ n1 n2))\n\n\
   (define (remainder n1 n2)\n\
  \  (- n1 (* (/ n1 n2) n2)))\n\n\
   (define (zero? x)\n\
  \  (= x 0))\n\n\
   (define (positive? x)\n\
  \  (> x 0))\n\n\
   (define (negative? x)\n\
  \  (< x 0))\n\n\
   (define (odd? x)\n\
  \  (= (remainder x 2) 1))\n\n\
   (define (even? x)\n\
  \  (= (remainder x 2) 0))\n\n\
   (define (min x y)\n\
  \  (if (< x y)\n\
  \    x\n\
  \    y))\n\n\
   (define (max x y)\n\
  \  (if (> x y)\n\
  \    x\n\
  \    y))\n\n\
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
   (define (length l)\n\
  \  (define (loop accumulated remaining)\n\
  \    (if (null? remaining)\n\
  \      accumulated\n\
  \      (loop (+ 1 accumulated) (cdr remaining))))\n\
  \  (loop 0 l))\n\n\
   ;; (list-ref '(a b c) 2) -> c\n\
   (define (list-ref l n)\n\
  \  (cond ((null? l) (error \"LIST-REF list is empty.\"))\n\
  \        ((= n 0) (car l))\n\
  \        ((< n 0) (error \"LIST-REF index must be >= 0.\"))\n\
  \        ((> n (- (length l) 1)) (error \"LIST-REF index out of bounds.\"))\n\
  \        (else (list-ref (cdr l) (- n 1)))))\n\n\
   (define (reverse l)\n\
  \  (define (loop accumulated remaining)\n\
  \    (if (null? remaining)\n\
  \      accumulated\n\
  \      (loop (cons (car remaining) accumulated)\n\
  \            (cdr remaining))))\n\
  \  (loop '() l))\n\n\
   (define (append . lists)\n\
  \  (define (append-lol lol)\n\
  \    (cond ((null? lol) '())\n\
  \          ((null? (cdr lol))\n\
  \           (car lol))\n\
  \          (else\n\
  \            (let ((first-list (car lol)))\n\
  \             (if (null? first-list)\n\
  \                 (append-lol (cdr lol))\n\
  \                 (cons (car first-list)\n\
  \                       (append-lol (cons (cdr first-list)\n\
  \                                         (cdr lol)))))))))\n\
  \  (append-lol lists))\n\n\
   (define (map f l)\n\
  \  (if (null? l)\n\
  \    '()\n\
  \    (cons (f (car l))\n\
  \          (map f (cdr l)))))\n\n\
   (define (filter f l)\n\
  \  (cond ((null? l) '())\n\
  \        ((f (car l))\n\
  \         (cons (car l)\n\
  \               (filter f (cdr l))))\n\
  \        (else\n\
  \          (filter f (cdr l)))))\n\n\
   ;; Tail recursive. Constant space use.\n\
   (define (foldl f acc l)\n\
  \  (if (null? l)\n\
  \    acc\n\
  \    (foldl f\n\
  \           (f acc (car l))\n\
  \           (cdr l))))\n\n\
   ;; Not tail recursive. Space use is proportional to length of list.\n\
   (define (foldr f acc l)\n\
  \  (if (null? l)\n\
  \    acc\n\
  \    (f (car l)\n\
  \       (foldr f acc (cdr l)))))\n\n\
   (define (for-each f l)\n\
  \  (cond ((null? l) '())\n\
  \         (else\n\
  \           (f (car l))\n\
  \           (for-each f (cdr l)))))\n\n\
   (define (member x l)\n\
  \  (cond ((null? l) #f)\n\
  \        ((equal? x (car l)) #t)\n\
  \        (else (member x (cdr l)))))\n\n\
   (define (index-of l x)\n\
  \  (define (loop l x acc)\n\
  \    (cond ((null? l) #f)\n\
  \          ((equal? (car l) x)\n\
  \           acc)\n\
  \          (else\n\
  \           (loop (cdr l) x (+ acc 1)))))\n\
  \  (loop l x 0))\n\n\
   ;; (assoc 2 '((1 a) (2 b))) -> '(2 b)\n\
   ;; (assoc 2 '((1 a))) -> #f\n\
   (define (assoc x al)\n\
  \  (cond ((null? al) #f)\n\
  \        ((equal? x (caar al)) (car al))\n\
  \        (else (assoc x (cdr al)))))\n\n\
   ;; (remove 2 '(1 2 3 2 1)) -> '(1 3 1)\n\
   (define (remove x l)\n\
  \  (filter (lambda (e) (not (equal? e x)))\n\
  \          l))\n\n\
   ;; (take '(1 2 3 4) 2) -> '(1 2)\n\
   (define (take l n)\n\
  \  (cond ((= n 0) '())\n\
  \        (else\n\
  \          (cons (car l)\n\
  \                (take (cdr l) (- n 1))))))\n\n\
   ;; (drop '(1 2 3 4) 2) -> '(3 4)\n\
   (define (drop l n)\n\
  \  (cond ((= n 0) l)\n\
  \        (else\n\
  \          (drop (cdr l) (- n 1)))))\n\
   (define list-tail drop)  ; R5RS procedure.\n\n\
   ;;; Display.\n\
   (define (newline)\n\
  \  (display \"\\n\"))\n\n\
   (define (displayln s)\n\
  \  (display s)\n\
  \  (newline))\n"
;;
