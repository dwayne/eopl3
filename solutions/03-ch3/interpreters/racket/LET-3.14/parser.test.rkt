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
 (parse "if zero?(z) then 1 else 0")
 (a-program (if-exp (zero?-exp (var-exp 'z))
                    (const-exp 1)
                    (const-exp 0))))

(check-equal?
 (parse "if zero?(2) then 0 else 1")
 (a-program (if-exp (zero?-exp (const-exp 2))
                    (const-exp 0)
                    (const-exp 1))))

(check-equal?
 (parse "let n=10 in -(n, 1)")
 (a-program (let-exp 'n (const-exp 10) (diff-exp (var-exp 'n) (const-exp 1)))))

(check-equal?
 (parse "minus(-(minus(5), 9))")
 (a-program (minus-exp (diff-exp (minus-exp (const-exp 5))
                                 (const-exp 9)))))

(check-equal?
 (parse "add(6, 2)")
 (a-program (add-exp (const-exp 6)
                     (const-exp 2))))

(check-equal?
 (parse "mul(6, 2)")
 (a-program (mul-exp (const-exp 6)
                     (const-exp 2))))

(check-equal?
 (parse "div(6, 2)")
 (a-program (div-exp (const-exp 6)
                     (const-exp 2))))

(check-equal?
 (parse "if equal?(1, 2) then 5 else 10")
 (a-program (if-exp (equal?-exp (const-exp 1) (const-exp 2))
                    (const-exp 5)
                    (const-exp 10))))

(check-equal?
 (parse "if greater?(1, 2) then 5 else 10")
 (a-program (if-exp (greater?-exp (const-exp 1) (const-exp 2))
                    (const-exp 5)
                    (const-exp 10))))

(check-equal?
 (parse "if less?(1, 2) then 5 else 10")
 (a-program (if-exp (less?-exp (const-exp 1) (const-exp 2))
                    (const-exp 5)
                    (const-exp 10))))
