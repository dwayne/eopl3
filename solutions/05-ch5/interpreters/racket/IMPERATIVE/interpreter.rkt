#lang eopl

(require "./env.rkt")
(require "./parser.rkt")

(provide

 ;; Expressed Values
 num-val bool-val

 ;; Interpreter
 run)

;; Registers:
(define exp 'uninitialized)   ;   exp : Exp
(define env 'uninitialized)   ;   env : Env
(define cont 'uninitialized)  ;  cont : Cont
(define val 'uninitialized)   ;   val : ExpVal
(define proc1 'uninitialized) ; proc1 : Proc

(define (run s)
  (let ([init-env (extend-env
                   'i (num-val 1)
                   (extend-env
                    'v (num-val 5)
                    (extend-env
                     'x (num-val 10)
                     (empty-env))))])
    (set! env init-env)
    (set! cont (end-cont))
    (value-of-program (parse s))))

;; Program -> FinalAnswer
;; Depends on: env, cont
(define (value-of-program prog)
  (cases program prog
    [a-program (exp1)
               (set! exp exp1)
               (value-of-exp)]))

;; () -> FinalAnswer
;; Depends on: exp, env, cont
(define (value-of-exp)
  (cases expression exp
    [const-exp (n)
               (set! val (num-val n))
               (apply-cont)]

    [var-exp (var)
             (set! val (apply-env env var construct-proc-val))
             (apply-cont)]

    [diff-exp (exp1 exp2)
              (set! exp exp1)
              (set! cont (diff1-cont exp2 env cont))
              (value-of-exp)]

    [zero?-exp (exp1)
               (set! exp exp1)
               (set! cont (zero1-cont cont))
               (value-of-exp)]

    [if-exp (exp1 exp2 exp3)
            (set! exp exp1)
            (set! cont (if-test-cont exp2 exp3 env cont))
            (value-of-exp)]

    [let-exp (var exp1 body)
             (set! exp exp1)
             (set! cont (let-exp-cont var body env cont))
             (value-of-exp)]

    [proc-exp (var body)
              (set! val (proc-val (procedure var body env)))
              (apply-cont)]

    [letrec-exp (proc-name bound-var proc-body letrec-body)
                (set! exp letrec-body)
                (set! env (extend-env-rec proc-name bound-var proc-body env))
                (value-of-exp)]

    [call-exp (rator rand)
              (set! exp rator)
              (set! cont (rator-cont rand env cont))
              (value-of-exp)]))

(define (construct-proc-val var body saved-env)
  (proc-val (procedure var body saved-env)))

;; Continuations
;;
;; It uses a data structure representation.

(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont
   (saved-cont continuation?)]
  [let-exp-cont
   (var identifier?)
   (body expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [diff1-cont
   (exp2 expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [diff2-cont
   (val1 expval?)
   (saved-cont continuation?)]
  [rator-cont
   (rand expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [rand-cont
   (proc-val expval?)
   (saved-cont continuation?)])

;; () -> FinalAnswer
;; Depends on: cont, val
(define (apply-cont)
  (cases continuation cont
    [end-cont ()
              (begin
                (eopl:printf "End of computation.~%")
                val)]

    [zero1-cont (saved-cont)
                (set! cont saved-cont)
                (set! val (bool-val (zero? (expval->num val))))
                (apply-cont)]

    [let-exp-cont (var body saved-env saved-cont)
                  (set! exp body)
                  (set! env (extend-env var val saved-env))
                  (set! cont saved-cont)
                  (value-of-exp)]

    [if-test-cont (exp2 exp3 saved-env saved-cont)
                  (if (expval->bool val)
                      (begin
                        (set! exp exp2)
                        (set! env saved-env)
                        (set! cont saved-cont)
                        (value-of-exp))
                      (begin
                        (set! exp exp3)
                        (set! env saved-env)
                        (set! cont saved-cont)
                        (value-of-exp)))]

    [diff1-cont (exp2 saved-env saved-cont)
                (set! exp exp2)
                (set! env saved-env)
                (set! cont (diff2-cont val saved-cont))
                (value-of-exp)]

    [diff2-cont (val1 saved-cont)
                (set! cont saved-cont)
                (set! val (num-val
                           (- (expval->num val1)
                              (expval->num val))))
                (apply-cont)]

    [rator-cont (rand saved-env saved-cont)
                (set! exp rand)
                (set! env saved-env)
                (set! cont (rand-cont val saved-cont))
                (value-of-exp)]

    [rand-cont (proc-val saved-cont)
               (set! proc1 (expval->proc proc-val))
               (set! val val)
               (set! cont saved-cont)
               (apply-procedure)]))

;; Procedure ADT

(define-datatype proc proc?
  [procedure
   (var identifier?)
   (body expression?)
   (saved-env env?)])

;; () -> FinalAnswer
;; Depends on: proc1, val, cont
(define (apply-procedure)
  (cases proc proc1
    [procedure (var body saved-env)
               (set! exp body)
               (set! env (extend-env var val saved-env))
               (value-of-exp)]))

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
