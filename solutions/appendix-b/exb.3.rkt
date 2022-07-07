#lang eopl

;; Exercise B.3
;;
;; Define an interpreter that takes the syntax tree produced
;; by the parser of exercise B.1 and evaluates it as an
;; arithmetic expression.

(require "./exb.1.rkt")

(provide interpret)

(define (interpret s)
  (interpret-arith-expr (parse s)))

(define (interpret-arith-expr expr)
  (cases arith-expr expr
    [sum (term ops terms) (interpret-sum term ops terms)]))

(define (interpret-sum term ops terms)
  (define (go result ops terms)
    (if (null? ops)
        result
        (cases additive-op (car ops)
          [add () (go (+ result (interpret-arith-term (car terms)))
                      (cdr ops)
                      (cdr terms))]
          [sub () (go (- result (interpret-arith-term (car terms)))
                      (cdr ops)
                      (cdr terms))])))
  (go (interpret-arith-term term) ops terms))

(define (interpret-arith-term term)
  (cases arith-term term
    [prod (factor ops factors) (interpret-prod factor ops factors)]))

(define (interpret-prod factor ops factors)
  (define (go result ops factors)
    (if (null? ops)
        result
        (cases multiplicative-op (car ops)
          [mul () (go (* result (interpret-arith-factor (car factors)))
                      (cdr ops)
                      (cdr factors))]
          [div () (go (/ result (interpret-arith-factor (car factors)))
                      (cdr ops)
                      (cdr factors))])))
  (go (interpret-arith-factor factor) ops factors))

(define (interpret-arith-factor factor)
  (cases arith-factor factor
    [const (num) num]
    [group (expr) (interpret-arith-expr expr)]))
