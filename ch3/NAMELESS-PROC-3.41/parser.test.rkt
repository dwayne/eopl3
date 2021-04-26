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
 (a-program (let-exp '(n)
                     (list (const-exp 10))
                     (diff-exp (var-exp 'n) (const-exp 1)))))

(check-equal?
 (parse "let a=1 b=2 in -(b, a)")
 (a-program (let-exp '(a b)
                     (list (const-exp 1) (const-exp 2))
                     (diff-exp (var-exp 'b) (var-exp 'a)))))

(check-equal?
 (parse "proc (x) -(x, 1)")
 (a-program (proc-exp '(x) (diff-exp (var-exp 'x)
                                     (const-exp 1)))))

(check-equal?
 (parse "(f x)")
 (a-program (call-exp (var-exp 'f)
                      (list (var-exp 'x)))))
