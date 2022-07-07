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

  (check-eq? (apply-nenv nenv 0 #f) 6)
  (check-eq? (apply-nenv nenv 1 #f) 8)
  (check-eq? (apply-nenv nenv 2 #f) 7)

  (check-exn #rx"Lexical address not found: 4" (lambda () (apply-nenv nenv 4 #f))))
