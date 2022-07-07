#lang eopl

(require "./env.rkt")
(require "./parser.rkt")
(require "./set.rkt")

(provide

 ;; Expressed Values
 num-val bool-val

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

    [diff-exp (exp1 exp2)
              (let ([val1 (value-of-exp exp1 env)]
                    [val2 (value-of-exp exp2 env)])
                (num-val
                 (- (expval->num val1)
                    (expval->num val2))))]

    [zero?-exp (exp1)
               (let ([val1 (value-of-exp exp1 env)])
                 (if (zero? (expval->num val1))
                     (bool-val #t)
                     (bool-val #f)))]

    [if-exp (exp1 exp2 exp3)
            (let ([val1 (value-of-exp exp1 env)])
              (if (expval->bool val1)
                  (value-of-exp exp2 env)
                  (value-of-exp exp3 env)))]

    [let-exp (var exp1 body)
             (let ([val1 (value-of-exp exp1 env)])
               (value-of-exp body (extend-env var val1 env)))]

    [proc-exp (var body)
              (let ([saved-env (keep-free-vars (free-variables exp) env)])
                (proc-val (procedure var body saved-env)))]

    [call-exp (rator rand)
              (let ([proc (expval->proc (value-of-exp rator env))]
                    [arg (value-of-exp rand env)])
                (apply-procedure proc arg))]))

(define (build-proc-env vars env)
  (if (null? vars)
      (empty-env)
      (extend-env (car vars)
                  (apply-env env (car vars))
                  (build-proc-env (cdr vars) env))))

(define (free-variables exp)
  (cases expression exp
    [const-exp (n)
               (empty-set)]

    [var-exp (var)
             (singleton-set var)]

    [diff-exp (exp1 exp2)
              (union-set (free-variables exp1)
                         (free-variables exp2))]

    [zero?-exp (exp1)
               (free-variables exp1)]

    [if-exp (exp1 exp2 exp3)
            (union-set (union-set (free-variables exp1)
                                  (free-variables exp2))
                       (free-variables exp3))]

    [let-exp (var exp1 body)
             (union-set (free-variables exp1)
                        (diff-set (free-variables body)
                                  (singleton-set var)))]

    [proc-exp (var body)
              (diff-set (free-variables body)
                        (singleton-set var))]

    [call-exp (rator rand)
              (union-set (free-variables rator)
                         (free-variables rand))]))

;; Procedure ADT

(define-datatype proc proc?
  [procedure
   (var identifier?)
   (body expression?)
   (saved-env env?)])

(define (apply-procedure proc1 val)
  (cases proc proc1
    [procedure (var body saved-env)
               (value-of-exp body (extend-env var val saved-env))]))

;; Values
;;
;; ExpVal = Int + Bool + Proc
;; DenVal = ExpVal

(define-datatype expval expval?
  [num-val (n number?)]
  [bool-val (b boolean?)]
  [proc-val (p proc?)])

(define (expval->num val)
  (cases expval val
    [num-val (n) n]
    [else (eopl:error 'expval->num "Not a number: ~s" val)]))

(define (expval->bool val)
  (cases expval val
    [bool-val (b) b]
    [else (eopl:error 'expval->bool "Not a boolean: ~s" val)]))

(define (expval->proc val)
  (cases expval val
    [proc-val (p) p]
    [else (eopl:error 'expval->proc "Not a procedure: ~s" val)]))
