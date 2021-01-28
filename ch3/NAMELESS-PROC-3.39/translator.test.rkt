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
let x = 17
in list(x)
CODE
   ))
 (a-program
  (nameless-let-exp
   (const-exp 17)
   (list-exp
    (list
     (nameless-var-exp 0))))))

(check-equal?
 (translate
  (parse
   #<<CODE
unpack x y = list(1, 2)
in -(y, x)
CODE
   ))
 (a-program
  (nameless-unpack-exp
   (list-exp (list (const-exp 1) (const-exp 2)))
   (diff-exp
    (nameless-var-exp 0)
    (nameless-var-exp 1)))))
