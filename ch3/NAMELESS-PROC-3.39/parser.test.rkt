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
 (parse "list()")
 (a-program (list-exp '())))

(check-equal?
 (parse "list(5)")
 (a-program (list-exp (list (const-exp 5)))))

(check-equal?
 (parse "list(5, y)")
 (a-program (list-exp (list (const-exp 5) (var-exp 'y)))))

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
 (parse "unpack x y z = list(1, 2, 3) in -(z, -(y, x))")
 (a-program (unpack-exp
             '(x y z)
             (list-exp (list (const-exp 1)
                             (const-exp 2)
                             (const-exp 3)))
             (diff-exp
              (var-exp 'z)
              (diff-exp
               (var-exp 'y)
               (var-exp 'x))))))

(check-equal?
 (parse "proc (x) -(x, 1)")
 (a-program (proc-exp 'x (diff-exp (var-exp 'x)
                                   (const-exp 1)))))

(check-equal?
 (parse "(f x)")
 (a-program (call-exp (var-exp 'f)
                      (var-exp 'x))))
