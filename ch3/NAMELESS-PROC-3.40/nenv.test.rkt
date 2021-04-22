#lang racket

(require "./nenv.rkt")

(require rackunit)

(let ([nenv (extend-nenv
             6
             (extend-nenv
              8
              (extend-nenv
               7
               (extend-nenv
                14
                (empty-nenv)))))])

  (check-eq? (apply-nenv nenv 0) 6)
  (check-eq? (apply-nenv nenv 1) 8)
  (check-eq? (apply-nenv nenv 2) 7)

  (check-exn #rx"Lexical address not found: 4" (lambda () (apply-nenv nenv 4))))
