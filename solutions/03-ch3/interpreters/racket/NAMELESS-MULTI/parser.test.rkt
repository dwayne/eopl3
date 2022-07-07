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
 (parse "let in a")
 (a-program (let-exp '()
                     '()
                     (var-exp 'a))))

(check-equal?
 (parse "let n=10 in -(n, 1)")
 (a-program (let-exp '(n)
                     (list (const-exp 10))
                     (diff-exp (var-exp 'n) (const-exp 1)))))

(check-equal?
 (parse "let a=1 b=2 c=3 in a")
 (a-program (let-exp '(a b c)
                     (list (const-exp 1) (const-exp 2) (const-exp 3))
                     (var-exp 'a))))

(check-equal?
 (parse "letrec sum(n) = if zero?(n) then 0 else -(n, -(0, (sum -(n, 1)))) in (sum 5)")
 (a-program (letrec-exp '(sum)
                        '((n))
                        (list (if-exp (zero?-exp (var-exp 'n))
                                      (const-exp 0)
                                      (diff-exp (var-exp 'n)
                                                (diff-exp (const-exp 0)
                                                          (call-exp (var-exp 'sum)
                                                                    (list (diff-exp (var-exp 'n)
                                                                                    (const-exp 1))))))))
                        (call-exp (var-exp 'sum)
                                  (list (const-exp 5))))))

(check-equal?
 (parse "letrec in 1")
 (a-program (letrec-exp '() '() '() (const-exp 1))))

(check-equal?
 (parse
  #<<CODE
letrec
  f(a) = a
  g(b) = b
  h(c) = c
in 1
CODE
  )
 (a-program (letrec-exp '(f g h)
                        '((a) (b) (c))
                        (list (var-exp 'a)
                              (var-exp 'b)
                              (var-exp 'c))
                        (const-exp 1))))

(check-equal?
 (parse "proc (x) -(x, 1)")
 (a-program (proc-exp '(x) (diff-exp (var-exp 'x)
                                     (const-exp 1)))))

(check-equal?
 (parse "proc () 1")
 (a-program (proc-exp '() (const-exp 1))))

(check-equal?
 (parse "proc (x, y, z) 1")
 (a-program (proc-exp '(x y z) (const-exp 1))))

(check-equal?
 (parse "(f x)")
 (a-program (call-exp (var-exp 'f)
                      (list (var-exp 'x)))))

(check-equal?
 (parse "(f)")
 (a-program (call-exp (var-exp 'f) '())))

(check-equal?
 (parse "(f x y z)")
 (a-program (call-exp (var-exp 'f) (list (var-exp 'x)
                                         (var-exp 'y)
                                         (var-exp 'z)))))
