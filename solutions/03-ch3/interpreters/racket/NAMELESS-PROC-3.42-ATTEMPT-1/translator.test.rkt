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
let x = 20
in let y = 30
   in let z = 40
      in let f = proc (a) v
         in (f 1)
CODE
   ))
 (a-program
  (nameless-let-exp
   (const-exp 20)
   (nameless-let-exp
    (const-exp 30)
    (nameless-let-exp
     (const-exp 40)
     (nameless-let-exp
      (nameless-proc-exp
       (nameless-var-exp 1))
      (call-exp
       (nameless-var-exp 0)
       (const-exp 1))))))))