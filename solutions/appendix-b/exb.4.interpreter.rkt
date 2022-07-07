#lang eopl

;; Exercise B.4
;;
;; Extend the interpreter to include variables.

(require "../ch2/ex2.5.rkt")
(require "./exb.4.parser.rkt")

(provide interpret)

(define (interpret s)
  (let ([init-env (extend-env
                   'x 5
                   (extend-env
                    'y 10
                    (extend-env
                     'z 15 (empty-env))))])
    (interpret-arith-expr (parse s) init-env)))

(define (interpret-arith-expr expr env)
  (cases arith-expr expr
    [sum (term ops terms) (interpret-sum term ops terms env)]))

(define (interpret-sum term ops terms env)
  (define (go result ops terms)
    (if (null? ops)
        result
        (cases additive-op (car ops)
          [add () (go (+ result (interpret-arith-term (car terms) env))
                      (cdr ops)
                      (cdr terms))]
          [sub () (go (- result (interpret-arith-term (car terms) env))
                      (cdr ops)
                      (cdr terms))])))
  (go (interpret-arith-term term env) ops terms))

(define (interpret-arith-term term env)
  (cases arith-term term
    [prod (factor ops factors) (interpret-prod factor ops factors env)]))

(define (interpret-prod factor ops factors env)
  (define (go result ops factors)
    (if (null? ops)
        result
        (cases multiplicative-op (car ops)
          [mul () (go (* result (interpret-arith-factor (car factors) env))
                      (cdr ops)
                      (cdr factors))]
          [div () (go (/ result (interpret-arith-factor (car factors) env))
                      (cdr ops)
                      (cdr factors))])))
  (go (interpret-arith-factor factor env) ops factors))

(define (interpret-arith-factor factor env)
  (cases arith-factor factor
    [const (num) num]
    [var (v) (apply-env env v)]
    [group (expr) (interpret-arith-expr expr env)]))
