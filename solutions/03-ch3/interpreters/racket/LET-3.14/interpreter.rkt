#lang eopl

(require "./env.rkt")
(require "./parser.rkt")

(provide

 ;; Interpreter
 run)

;; Values
;;
;; ExpVal = Int
;; DenVal = ExpVal

(define (run s)
  (let ([init-env (extend-env
                   'i 1
                   (extend-env
                    'v 5
                    (extend-env
                     'x 10
                     (empty-env))))])
    (value-of-program (parse s) init-env)))

(define (value-of-program prog env)
  (cases program prog
    [a-program (exp) (value-of-exp exp env)]))

(define (value-of-exp exp env)
  (cases expression exp
    [const-exp (n) n]

    [var-exp (var)
             (apply-env env var)]

    [add-exp (exp1 exp2)
             (let ([val1 (value-of-exp exp1 env)]
                   [val2 (value-of-exp exp2 env)])
               (+ val1 val2))]

    [diff-exp (exp1 exp2)
              (let ([val1 (value-of-exp exp1 env)]
                    [val2 (value-of-exp exp2 env)])
                (- val1 val2))]

    [mul-exp (exp1 exp2)
             (let ([val1 (value-of-exp exp1 env)]
                   [val2 (value-of-exp exp2 env)])
               (* val1 val2))]

    [div-exp (exp1 exp2)
             (let ([val1 (value-of-exp exp1 env)]
                   [val2 (value-of-exp exp2 env)])
               (if (zero? val2)
                   (eopl:error 'div "division by 0 is undefined")
                   (quotient val1 val2)))]

    [minus-exp (exp1)
               (let ([val1 (value-of-exp exp1 env)])
                 (- val1))]

    [if-exp (exp1 exp2 exp3)
            (let ([val1 (value-of-bool-exp exp1 env)])
              (if val1
                  (value-of-exp exp2 env)
                  (value-of-exp exp3 env)))]

    [let-exp (var exp1 body)
             (let ([val1 (value-of-exp exp1 env)])
               (value-of-exp body (extend-env var val1 env)))]))

(define (value-of-bool-exp exp env)
  (cases bool-exp exp
    [zero?-exp (exp1)
               (let ([val1 (value-of-exp exp1 env)])
                 (zero? val1))]

    [equal?-exp (exp1 exp2)
                (let ([val1 (value-of-exp exp1 env)]
                      [val2 (value-of-exp exp2 env)])
                  (= val1 val2))]

    [greater?-exp (exp1 exp2)
                  (let ([val1 (value-of-exp exp1 env)]
                        [val2 (value-of-exp exp2 env)])
                    (> val1 val2))]

    [less?-exp (exp1 exp2)
               (let ([val1 (value-of-exp exp1 env)]
                     [val2 (value-of-exp exp2 env)])
                 (< val1 val2))]))
