#lang eopl

(require "./env.rkt")
(require "./parser.rkt")

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
    (value-of-program (parse s) init-env (end-cont))))

;; Program x Env x Cont -> FinalAnswer
(define (value-of-program prog env cont)
  (cases program prog
    [a-program (exp) (value-of-exp exp env cont)]))

;; Expression x Env x Cont -> FinalAnswer
(define (value-of-exp exp env cont)
  (cases expression exp
    [const-exp (n)
               (apply-cont cont (num-val n))]

    [var-exp (var)
             (apply-cont cont (apply-env env var construct-proc-val))]

    [diff-exp (exp1 exp2)
              (value-of-exp exp1 env (diff1-cont exp2 env cont))]

    [zero?-exp (exp1)
               (value-of-exp exp1 env (zero1-cont cont))]

    [if-exp (exp1 exp2 exp3)
            (value-of-exp exp1 env (if-test-cont exp2 exp3 env cont))]

    [let-exp (var exp1 body)
             (value-of-exp exp1 env (let-exp-cont var body env cont))]

    [proc-exp (var body)
              (apply-cont cont (proc-val (procedure var body env)))]

    [letrec-exp (proc-name bound-var proc-body letrec-body)
                (value-of-exp
                 letrec-body
                 (extend-env-rec proc-name bound-var proc-body env)
                 cont)]

    [call-exp (rator rand)
              (value-of-exp rator env (rator-cont rand env cont))]))

(define (construct-proc-val var body saved-env)
  (proc-val (procedure var body saved-env)))

;; Continuations
;;
;; It uses a data structure representation.

(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont
   (cont continuation?)]
  [let-exp-cont
   (var identifier?)
   (body expression?)
   (env env?)
   (cont continuation?)]
  [if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env env?)
   (cont continuation?)]
  [diff1-cont
   (exp2 expression?)
   (env env?)
   (cont continuation?)]
  [diff2-cont
   (val1 expval?)
   (cont continuation?)]
  [rator-cont
   (rand expression?)
   (env env?)
   (cont continuation?)]
  [rand-cont
   (proc-val expval?)
   (cont continuation?)])

;; Cont x ExpVal -> FinalAnswer
(define (apply-cont c val)
  (cases continuation c
    [end-cont ()
              (begin
                (eopl:printf "End of computation.~%")
                val)]

    [zero1-cont (cont)
                (apply-cont cont (bool-val (zero? (expval->num val))))]

    [let-exp-cont (var body env cont)
                  (value-of-exp body (extend-env var val env) cont)]

    [if-test-cont (exp2 exp3 env cont)
                  (if (expval->bool val)
                      (value-of-exp exp2 env cont)
                      (value-of-exp exp3 env cont))]

    [diff1-cont (exp2 env cont)
                (value-of-exp exp2 env (diff2-cont val cont))]

    [diff2-cont (val1 cont)
                (apply-cont
                 cont
                 (num-val
                  (- (expval->num val1)
                     (expval->num val))))]

    [rator-cont (rand env cont)
                (value-of-exp rand env (rand-cont val cont))]

    [rand-cont (proc-val cont)
               (apply-procedure (expval->proc proc-val) val cont)]))

;; Procedure ADT

(define-datatype proc proc?
  [procedure
   (var identifier?)
   (body expression?)
   (saved-env env?)])

(define (apply-procedure proc1 val cont)
  (cases proc proc1
    [procedure (var body saved-env)
               (value-of-exp body (extend-env var val saved-env) cont)]))

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
