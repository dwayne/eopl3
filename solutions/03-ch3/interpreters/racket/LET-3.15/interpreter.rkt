#lang eopl

(require "./env.rkt")
(require "./parser.rkt")

(provide

 ;; Expressed Values
 num-val bool-val list-val

 ;; Interpreter
 run)

(define (run s)
  (let ([init-env (extend-env
                   'i (num-val 1)
                   (extend-env
                    'v (num-val 5)
                    (extend-env
                     'x (num-val 10)
                     (empty-env))))])
    (value-of-program (parse s) init-env)))

(define (value-of-program prog env)
  (cases program prog
    [a-program (exp) (value-of-exp exp env)]))

(define (value-of-exp exp env)
  (cases expression exp
    [const-exp (n)
               (num-val n)]

    [var-exp (var)
             (apply-env env var)]

    [add-exp (exp1 exp2)
             (let ([val1 (value-of-exp exp1 env)]
                   [val2 (value-of-exp exp2 env)])
               (num-val
                (+ (expval->num val1)
                   (expval->num val2))))]

    [diff-exp (exp1 exp2)
              (let ([val1 (value-of-exp exp1 env)]
                    [val2 (value-of-exp exp2 env)])
                (num-val
                 (- (expval->num val1)
                    (expval->num val2))))]

    [mul-exp (exp1 exp2)
             (let ([val1 (value-of-exp exp1 env)]
                   [val2 (value-of-exp exp2 env)])
               (num-val
                (* (expval->num val1)
                   (expval->num val2))))]

    [div-exp (exp1 exp2)
             (let ([val1 (value-of-exp exp1 env)]
                   [val2 (value-of-exp exp2 env)])
               (if (zero? (expval->num val2))
                   (eopl:error 'div "division by 0 is undefined")
                   (num-val
                    (quotient (expval->num val1)
                              (expval->num val2)))))]

    [minus-exp (exp1)
               (let ([val1 (value-of-exp exp1 env)])
                 (num-val
                  (- (expval->num val1))))]

    [zero?-exp (exp1)
               (let ([val1 (value-of-exp exp1 env)])
                 (if (zero? (expval->num val1))
                     (bool-val #t)
                     (bool-val #f)))]

    [equal?-exp (exp1 exp2)
                (let ([val1 (value-of-exp exp1 env)]
                      [val2 (value-of-exp exp2 env)])
                  (bool-val
                   (= (expval->num val1)
                      (expval->num val2))))]

    [greater?-exp (exp1 exp2)
                  (let ([val1 (value-of-exp exp1 env)]
                        [val2 (value-of-exp exp2 env)])
                    (bool-val
                     (> (expval->num val1)
                        (expval->num val2))))]

    [less?-exp (exp1 exp2)
               (let ([val1 (value-of-exp exp1 env)]
                     [val2 (value-of-exp exp2 env)])
                 (bool-val
                  (< (expval->num val1)
                     (expval->num val2))))]

    [cons-exp (exp1 exp2)
              (let ([val1 (value-of-exp exp1 env)]
                    [val2 (value-of-exp exp2 env)])
                (list-val
                 (cons val1
                       (expval->list val2))))]

    [car-exp (exp1)
             (let ([val1 (value-of-exp exp1 env)])
               (if (null? (expval->list val1))
                   (eopl:error 'car "List is empty")
                   (car (expval->list val1))))]

    [cdr-exp (exp1)
             (let ([val1 (value-of-exp exp1 env)])
               (if (null? (expval->list val1))
                   (eopl:error 'cdr "List is empty")
                   (list-val (cdr (expval->list val1)))))]

    [null?-exp (exp1)
               (let ([val1 (value-of-exp exp1 env)])
                 (bool-val (null? (expval->list val1))))]

    [emptylist-exp ()
                   (list-val '())]

    [list-exp (exps)
              (list-val (map (lambda (exp)
                               (value-of-exp exp env))
                             exps))]

    [if-exp (exp1 exp2 exp3)
            (let ([val1 (value-of-exp exp1 env)])
              (if (expval->bool val1)
                  (value-of-exp exp2 env)
                  (value-of-exp exp3 env)))]

    [cond-exp (conditions exps)
              (if (null? conditions)
                  (eopl:error 'cond "No condition is satisfied")
                  (let ([val1 (value-of-exp (car conditions) env)])
                    (if (expval->bool val1)
                        (value-of-exp (car exps) env)
                        (value-of-exp (cond-exp (cdr conditions)
                                                (cdr exps))
                                      env))))]

    [let-exp (var exp1 body)
             (let ([val1 (value-of-exp exp1 env)])
               (value-of-exp body (extend-env var val1 env)))]

    [print-exp (exp1)
               (let ([val1 (value-of-exp exp1 env)])
                 (display-expval val1)
                 (display "\n")
                 (num-val 1))]))

(define (display-expval val)
  (cases expval val
    [num-val (n) (display n)]
    [bool-val (b) (display b)]
    [list-val (l) (display-list l)]))

(define (display-list l)
  (display "(")
  (display-list-elements l)
  (display ")"))

(define (display-list-elements l)
  (if (null? l)
      (display "")
      (begin (display-expval (car l))
             (if (null? (cdr l)) (display "") (display " "))
             (display-list-elements (cdr l)))))

;; Values
;;
;; ExpVal = Int + Bool
;; DenVal = ExpVal

(define-datatype expval expval?
  [num-val (n number?)]
  [bool-val (b boolean?)]
  [list-val (l list?)])

(define (expval->num val)
  (cases expval val
    [num-val (n) n]
    [else (eopl:error 'expval->num "Not a number: ~s" val)]))

(define (expval->bool val)
  (cases expval val
    [bool-val (b) b]
    [else (eopl:error 'expval->bool "Not a boolean: ~s" val)]))

(define (expval->list val)
  (cases expval val
    [list-val (l) l]
    [else (eopl:error 'expval->list "Not a list: ~s" val)]))
