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
letrec double(x)
  = if zero?(x) then 0 else -((double -(x, 1)), -(0, 2))
in (double 6)
CODE
   ))
 (a-program
  (nameless-letrec-exp
   (if-exp
    (zero?-exp (nameless-var-exp 0))
    (const-exp 0)
    (diff-exp
     (call-exp
      (nameless-letrec-var-exp 1)
      (diff-exp (nameless-var-exp 0) (const-exp 1)))
     (diff-exp (const-exp 0) (const-exp 2))))
   (call-exp
    (nameless-letrec-var-exp 0)
    (const-exp 6)))))

(check-equal?
 (translate
  (parse
   #<<CODE
let x = 6
in let y = 2
   in letrec double(x)
        = if zero?(x) then 0 else -((double -(x, 1)), -(0, y))
      in (double x)
CODE
   ))
 (a-program
  (nameless-let-exp
   (const-exp 6)
   (nameless-let-exp
    (const-exp 2)
    (nameless-letrec-exp
     (if-exp
      (zero?-exp (nameless-var-exp 0))
      (const-exp 0)
      (diff-exp
       (call-exp
        (nameless-letrec-var-exp 1)
        (diff-exp (nameless-var-exp 0) (const-exp 1)))
       (diff-exp (const-exp 0) (nameless-var-exp 2))))
     (call-exp
      (nameless-letrec-var-exp 0)
      (nameless-var-exp 2)))))))
