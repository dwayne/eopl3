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
   (list (const-exp 37))
   (nameless-proc-exp
    (nameless-let-exp
     (list
      (diff-exp
       (nameless-var-exp 0 0)
       (nameless-var-exp 1 0)))
     (diff-exp
      (nameless-var-exp 2 0)
      (nameless-var-exp 1 0)))))))
