#lang racket

(require "./parser.rkt")
(require "./translator.rkt")

(require rackunit)

(check-equal?
 (translate
  (parse
   #<<CODE
let x = 37
in proc (y)
     let z = -(y, x)
     in -(x, y)
CODE
   ))
 (a-program
  (nameless-let-exp
   (const-exp 37)
   (nameless-proc-exp
    (nameless-let-exp
     (diff-exp
      (nameless-var-exp 0)
      (nameless-var-exp 1))
     (diff-exp
      (nameless-var-exp 2)
      (nameless-var-exp 1)))))))

(check-equal?
 (translate
  (parse
   #<<CODE
letrec sum(n) = if zero?(n)
                then 0
                else -(n, -(0, (sum -(n, 1))))
in (sum 5)
CODE
   ))
 (a-program
  (nameless-letrec-exp
   (if-exp (zero?-exp (nameless-var-exp 0))
           (const-exp 0)
           (diff-exp (nameless-var-exp 0)
                     (diff-exp (const-exp 0)
                               (call-exp (nameless-var-exp 1)
                                         (diff-exp (nameless-var-exp 0)
                                                   (const-exp 1))))))
   (call-exp (nameless-var-exp 0) (const-exp 5)))))