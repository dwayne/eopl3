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
 (a-program (let-exp (list 'n)
                     (list (const-exp 10))
                     (diff-exp (var-exp 'n) (const-exp 1)))))

(check-equal?
 (parse
  #<<LET
let x = 30
in let x = -(x,1)
       y = -(x,2)
   in -(x,y)
LET
  )
 (a-program (let-exp (list 'x)
                     (list (const-exp 30))
                     (let-exp (list 'x 'y)
                              (list (diff-exp (var-exp 'x) (const-exp 1))
                                    (diff-exp (var-exp 'x) (const-exp 2)))
                              (diff-exp (var-exp 'x)
                                        (var-exp 'y))))))

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
 (parse "equal?(1, 2)")
 (a-program (equal?-exp (const-exp 1)
                        (const-exp 2))))

(check-equal?
 (parse "greater?(1, 2)")
 (a-program (greater?-exp (const-exp 1)
                          (const-exp 2))))

(check-equal?
 (parse "less?(1, 2)")
 (a-program (less?-exp (const-exp 1)
                       (const-exp 2))))

(check-equal?
 (parse "emptylist")
 (a-program (emptylist-exp)))

(check-equal?
 (parse "cons(5, emptylist)")
 (a-program (cons-exp (const-exp 5)
                      (emptylist-exp))))

(check-equal?
 (parse "car(cons(x, emptylist))")
 (a-program (car-exp (cons-exp (var-exp 'x)
                               (emptylist-exp)))))

(check-equal?
 (parse "cdr(cons(x, emptylist))")
 (a-program (cdr-exp (cons-exp (var-exp 'x)
                               (emptylist-exp)))))

(check-equal?
 (parse "if null?(emptylist) then 0 else 1")
 (a-program (if-exp (null?-exp (emptylist-exp))
                    (const-exp 0)
                    (const-exp 1))))

(check-equal?
 (parse "list()")
 (a-program (list-exp '())))

(check-equal?
 (parse "list(1)")
 (a-program (list-exp (list (const-exp 1)))))

(check-equal?
 (parse "list(1, if zero?(i) then x else -(0, x))")
 (a-program (list-exp (list (const-exp 1)
                            (if-exp (zero?-exp (var-exp 'i))
                                    (var-exp 'x)
                                    (diff-exp (const-exp 0)
                                              (var-exp 'x)))))))

(check-equal?
 (parse "cond end")
 (a-program (cond-exp '() '())))

(check-equal?
 (parse
  #<<LET
cond
  equal?(x, i)   ==> 1
  greater?(x, i) ==> 2
  less?(x, i)    ==> 3
end
LET
  )
 (a-program (cond-exp
             (list (equal?-exp (var-exp 'x) (var-exp 'i))
                   (greater?-exp (var-exp 'x) (var-exp 'i))
                   (less?-exp (var-exp 'x) (var-exp 'i)))
             (list (const-exp 1)
                   (const-exp 2)
                   (const-exp 3)))))
