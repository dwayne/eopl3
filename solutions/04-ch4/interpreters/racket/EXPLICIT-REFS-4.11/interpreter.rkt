#lang eopl

(require "./env.rkt")
(require "./parser.rkt")
(require "./store.rkt")

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
    (initialize-store!)
    (value-of-program (parse s) init-env)))

(define (value-of-program prog env)
  (cases program prog
    [a-program (exp) (value-of-exp exp env)]))

(define (value-of-exp exp env)
  (cases expression exp
    [const-exp (n)
               (num-val n)]

    [var-exp (var)
             (apply-env env var construct-proc-val)]

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
              (proc-val (procedure var body env))]

    [letrec-exp (proc-names bound-vars proc-bodies letrec-body)
                (value-of-exp letrec-body (extend-env-rec proc-names bound-vars proc-bodies env))]

    [call-exp (rator rand)
              (let ([proc (expval->proc (value-of-exp rator env))]
                    [arg (value-of-exp rand env)])
                (apply-procedure proc arg))]

    [begin-exp (exp1 exps)
               (value-of-begin-exp (cons exp1 exps) env)]

    [newref-exp (exp1)
                (let ([val (value-of-exp exp1 env)])
                  (ref-val (newref val)))]

    [deref-exp (exp1)
               (let ([ref (expval->ref (value-of-exp exp1 env))])
                 (deref ref))]

    [setref-exp (exp1 exp2)
                (let ([ref (expval->ref (value-of-exp exp1 env))]
                      [val (value-of-exp exp2 env)])
                  (setref! ref val)
                  (num-val 23))]

    [list-exp (exps)
              (list-val (value-of-list-exp exps env))]))

(define (value-of-begin-exp exps env)
  (if (null? (cdr exps))
      (value-of-exp (car exps) env)
      (begin
        (value-of-exp (car exps) env)
        (value-of-begin-exp (cdr exps) env))))

(define (value-of-list-exp exps env)
  (map (lambda (exp) (value-of-exp exp env)) exps))

(define (construct-proc-val var body saved-env)
  (proc-val (procedure var body saved-env)))

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
;; ExpVal = Int + Bool + Proc + Ref(ExpVal) + List(ExpVal)
;; DenVal = ExpVal

(define-datatype expval expval?
  [num-val (n number?)]
  [bool-val (b boolean?)]
  [proc-val (p proc?)]
  [ref-val (r reference?)]
  [list-val (l list?)])

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

(define (expval->ref val)
  (cases expval val
    [ref-val (r) r]
    [else (eopl:error 'expval->ref "Not a reference: ~s" val)]))

(define (expval->list val)
  (cases expval val
    [list-val (l) l]
    [else (eopl:error 'expval->list "Not a list: ~s" val)]))