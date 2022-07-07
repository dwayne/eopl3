#lang racket

(require "./parser.rkt")

(require rackunit)

(check-equal?
 (parse "1")
 (a-program (const-exp 1)))

(check-equal?
 (parse "x")
 (a-program (var-exp 'x)))

(check-equal?
 (parse "-(5, y)")
 (a-program (diff-exp (const-exp 5) (var-exp 'y))))

(check-equal?
 (parse "zero?(z)")
 (a-program (zero?-exp (var-exp 'z))))

(check-equal?
 (parse "if zero?(2) then 0 else 1")
 (a-program (if-exp (zero?-exp (const-exp 2))
                    (const-exp 0)
                    (const-exp 1))))

(check-equal?
 (parse "let n=10 in -(n, 1)")
 (a-program (let-exp 'n
                     (const-exp 10)
                     (diff-exp (var-exp 'n) (const-exp 1)))))

(check-equal?
 (parse "proc (x) -(x, 1)")
 (a-program (proc-exp 'x (diff-exp (var-exp 'x)
                                   (const-exp 1)))))

(check-equal?
 (parse "letrec f(x) = a in b")
 (a-program (letrec-exp (list 'f)
                        (list (list 'x))
                        (list (var-exp 'a))
                        (var-exp 'b))))

(check-equal?
 (parse
  #<<CODE
letrec
  f(x) = (g x)
  g(y) = (f y)
in (f 1)
CODE
  )
 (a-program (letrec-exp (list 'f 'g)
                        (list (list 'x)
                              (list 'y))
                        (list (call-exp (var-exp 'g) (list (var-exp 'x)))
                              (call-exp (var-exp 'f) (list (var-exp 'y))))
                        (call-exp (var-exp 'f) (list (const-exp 1))))))

(check-equal?
 (parse "(f x)")
 (a-program (call-exp (var-exp 'f)
                      (list (var-exp 'x)))))

(check-equal?
 (parse "letrec f(x,y) = 1 in (f 1 2)")
 (a-program (letrec-exp (list 'f)
                        (list (list 'x 'y))
                        (list (const-exp 1))
                        (call-exp (var-exp 'f) (list (const-exp 1) (const-exp 2))))))
