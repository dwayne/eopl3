#lang racket

(require "./ex2.31.rkt")

(require rackunit)

(check-equal?
 (parse-prefix-list '(1))
 (list (const-exp 1)))

(check-equal?
 (parse-prefix-list '(- 2 1))
 (list (diff-exp (const-exp 2)
                 (const-exp 1))))

(check-equal?
 (parse-prefix-list '(- - 5 3 - 10 9))
 (list (diff-exp (diff-exp (const-exp 5)
                           (const-exp 3))
                 (diff-exp (const-exp 10)
                           (const-exp 9)))))

(check-equal?
 (parse-prefix-list '(- - 3 2 - 4 - 12 7))
 (list (diff-exp (diff-exp (const-exp 3)
                           (const-exp 2))
                 (diff-exp (const-exp 4)
                           (diff-exp (const-exp 12)
                                     (const-exp 7))))))
