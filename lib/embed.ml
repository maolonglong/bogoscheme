let primitives_dot_scm =
  "(define not\n\
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
  \        y)))\n"
;;
