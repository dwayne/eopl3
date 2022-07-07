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
let x = 1
in let y = 0
   in cond
        zero?(x) ==> y
        zero?(y) ==> x
      end
CODE
   ))
 (a-program
  (nameless-let-exp
   (const-exp 1)
   (nameless-let-exp
    (const-exp 0)
    (cond-exp
     (list (zero?-exp (nameless-var-exp 1))
           (zero?-exp (nameless-var-exp 0)))
     (list (nameless-var-exp 0)
           (nameless-var-exp 1)))))))
