#lang racket

(require "./parser.rkt")

(require rackunit)

(check-equal?
 (parse "f = 1")
 (a-program (assign-stmt 'f (const-exp 1))))

(check-equal?
 (parse "print 1")
 (a-program (print-stmt (const-exp 1))))

(check-equal?
 (parse "{}")
 (a-program (block-stmt '())))

(check-equal?
 (parse "{ f = 1 }")
 (a-program (block-stmt (list (assign-stmt 'f (const-exp 1))))))

(check-equal?
 (parse "{ f = 1; print f }")
 (a-program (block-stmt (list (assign-stmt 'f (const-exp 1))
                              (print-stmt (var-exp 'f))))))

(check-equal?
 (parse "if not(x) print 1 print 2")
 (a-program (if-stmt (not-exp (var-exp 'x))
                     (print-stmt (const-exp 1))
                     (print-stmt (const-exp 2)))))

(check-equal?
 (parse "while b f = 1")
 (a-program (while-stmt (var-exp 'b)
                        (assign-stmt 'f (const-exp 1)))))

(check-equal?
 (parse "var; {}")
 (a-program (var-stmt '() (block-stmt '()))))

(check-equal?
 (parse "var a; {}")
 (a-program (var-stmt '(a) (block-stmt '()))))

(check-equal?
 (parse "var a, b, c, d; {}")
 (a-program (var-stmt '(a b c d) (block-stmt '()))))

(check-equal?
 (parse "var x, y; print x")
 (a-program (var-stmt '(x y) (print-stmt (var-exp 'x)))))

(check-equal?
 (parse "f = x")
 (a-program (assign-stmt 'f (var-exp 'x))))

(check-equal?
 (parse "f = -(5, y)")
 (a-program (assign-stmt 'f (diff-exp (const-exp 5) (var-exp 'y)))))

(check-equal?
 (parse "f = +(5, y)")
 (a-program (assign-stmt 'f (add-exp (const-exp 5) (var-exp 'y)))))

(check-equal?
 (parse "f = *(5, y)")
 (a-program (assign-stmt 'f (mul-exp (const-exp 5) (var-exp 'y)))))

(check-equal?
 (parse "f = zero?(z)")
 (a-program (assign-stmt 'f (zero?-exp (var-exp 'z)))))

(check-equal?
 (parse "f = not(z)")
 (a-program (assign-stmt 'f (not-exp (var-exp 'z)))))

(check-equal?
 (parse "f = if zero?(2) then 0 else 1")
 (a-program (assign-stmt 'f (if-exp (zero?-exp (const-exp 2))
                                    (const-exp 0)
                                    (const-exp 1)))))

(check-equal?
 (parse "f = let n=10 in -(n, 1)")
 (a-program (assign-stmt 'f (let-exp 'n
                                     (const-exp 10)
                                     (diff-exp (var-exp 'n) (const-exp 1))))))

(check-equal?
 (parse "f = proc (x) -(x, 1)")
 (a-program (assign-stmt 'f (proc-exp 'x (diff-exp (var-exp 'x)
                                                   (const-exp 1))))))

(check-equal?
 (parse "f = letrec f(x) = a in b")
 (a-program (assign-stmt 'f (letrec-exp '(f)
                                        '(x)
                                        (list (var-exp 'a))
                                        (var-exp 'b)))))

(check-equal?
 (parse "f = letrec f(x) = (g x) g(y) = y in (f 2)")
 (a-program (assign-stmt 'f (letrec-exp '(f g)
                                        '(x y)
                                        (list
                                         (call-exp (var-exp 'g)
                                                   (var-exp 'x))
                                         (var-exp 'y))
                                        (call-exp (var-exp 'f)
                                                  (const-exp 2))))))

(check-equal?
 (parse "f = (f x)")
 (a-program (assign-stmt 'f (call-exp (var-exp 'f)
                                      (var-exp 'x)))))

(check-equal?
 (parse "f = begin 5; 4; 3 end")
 (a-program (assign-stmt 'f (begin-exp
                              (const-exp 5)
                              (list
                               (const-exp 4)
                               (const-exp 3))))))

(check-equal?
 (parse "f = set x = 1")
 (a-program (assign-stmt 'f (assign-exp 'x (const-exp 1)))))
