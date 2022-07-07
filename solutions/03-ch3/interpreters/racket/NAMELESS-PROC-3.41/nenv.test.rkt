#lang racket

(require "./nenv.rkt")

(require rackunit)

(let ([nenv (extend-nenv (list 6 8)
                         (extend-nenv (list 7 14)
                                      (empty-nenv)))])
  (check-eq? (apply-nenv nenv 0 0) 6)
  (check-eq? (apply-nenv nenv 0 1) 8)
  (check-eq? (apply-nenv nenv 1 0) 7)

  (check-exn #rx"Lexical address not found: 1 2" (lambda () (apply-nenv nenv 1 2))))
