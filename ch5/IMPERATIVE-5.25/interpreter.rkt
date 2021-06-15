#lang eopl

(require "./env.rkt")
(require "./parser.rkt")

(provide

 ;; Expressed Values
 num-val bool-val list-val

 ;; Interpreter
 run)

;; Registers

(define exp 'uninitialized)   ;   exp : Exp
(define exps 'uninitialized)  ;  exps : List(Exp)
(define env 'uninitialized)   ;   env : Env
(define cont 'uninitialized)  ;  cont : Cont
(define val 'uninitialized)   ;   val : ExpVal
(define vals 'uninitialized)  ;  vals : List(ExpVal)
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

    [cons-exp (exp1 exp2)
              (set! exp exp1)
              (set! cont (cons1-cont exp2 env cont))
              (value-of-exp)]

    [car-exp (exp1)
             (set! exp exp1)
             (set! cont (car-cont cont))
             (value-of-exp)]

    [cdr-exp (exp1)
             (set! exp exp1)
             (set! cont (cdr-cont cont))
             (value-of-exp)]

    [null?-exp (exp1)
               (set! exp exp1)
               (set! cont (null-cont cont))
               (value-of-exp)]

    [emptylist-exp ()
                   (set! val (list-val '()))
                   (apply-cont)]

    [list-exp (exps1)
              (set! exps exps1)
              (value-of-exps)]

    [if-exp (exp1 exp2 exp3)
            (set! exp exp1)
            (set! cont (if-test-cont exp2 exp3 env cont))
            (value-of-exp)]

    [let-exp (vars exps1 body)
             (set! exps exps1)
             (set! cont (let-exps-cont vars body env cont))
             (value-of-exps)]

    [proc-exp (vars body)
              (set! val (proc-val (procedure vars body env)))
              (apply-cont)]

    [letrec-exp (proc-name bound-var proc-body letrec-body)
                (set! exp letrec-body)
                (set! env (extend-env-rec proc-name bound-var proc-body env))
                (value-of-exp)]

    [call-exp (rator rands)
              (set! exp rator)
              (set! cont (rator-cont rands env cont))
              (value-of-exp)]))

(define (construct-proc-val vars body saved-env)
  (proc-val (procedure vars body saved-env)))

;; () -> FinalAnswer
;; Depends on: exps, env, cont
(define (value-of-exps)
  (if (null? exps)
      (begin
        (set! val (list-val '()))
        (apply-cont))
      (begin
        (set! exp (car exps))
        (set! cont (head-cont (cdr exps) env cont))
        (value-of-exp))))

;; Continuations
;;
;; It uses a data structure representation.

(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont
   (saved-cont continuation?)]
  [let-exps-cont
   (vars list?)
   (body expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [cons1-cont
   (exp2 expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [cons2-cont
   (val1 expval?)
   (saved-cont continuation?)]
  [car-cont
   (saved-cont continuation?)]
  [cdr-cont
   (saved-cont continuation?)]
  [null-cont
   (saved-cont continuation?)]
  [rator-cont
   (rands list?)
   (saved-env env?)
   (saved-cont continuation?)]
  [rands-cont
   (proc-val expval?)
   (saved-cont continuation?)]
  [head-cont
   (tail list?)
   (saved-env env?)
   (saved-cont continuation?)]
  [tail-cont
   (head expval?)
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

    [let-exps-cont (vars body saved-env saved-cont)
                   (set! exp body)
                   (set! env (extend-env-parallel vars (expval->list val) saved-env))
                   (set! cont saved-cont)
                   (value-of-exp)]

    [cons1-cont (exp2 saved-env saved-cont)
                (set! exp exp2)
                (set! env saved-env)
                (set! cont (cons2-cont val saved-cont))
                (value-of-exp)]

    [cons2-cont (val1 saved-cont)
                (set! cont saved-cont)
                (set! val (list-val (cons val1 (expval->list val))))
                (apply-cont)]

    [car-cont (saved-cont)
              (set! cont saved-cont)
              (set! val (car (expval->list val)))
              (apply-cont)]

    [cdr-cont (saved-cont)
              (set! cont saved-cont)
              (set! val (list-val (cdr (expval->list val))))
              (apply-cont)]

    [null-cont (saved-cont)
               (set! cont saved-cont)
               (set! val (bool-val (null? (expval->list val))))
               (apply-cont)]

    [rator-cont (rands saved-env saved-cont)
                (set! exps rands)
                (set! env saved-env)
                (set! cont (rands-cont val saved-cont))
                (value-of-exps)]

    [rands-cont (proc-val saved-cont)
                (set! proc1 (expval->proc proc-val))
                (set! vals (expval->list val))
                (set! cont saved-cont)
                (apply-procedure)]

    [head-cont (tail saved-env saved-cont)
               (set! exps tail)
               (set! env saved-env)
               (set! cont (tail-cont val saved-cont))
               (value-of-exps)]

    [tail-cont (head saved-cont)
               (set! cont saved-cont)
               (set! val  (list-val (cons head (expval->list val))))
               (apply-cont)]

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
                (apply-cont)]))

;; Procedure ADT

(define-datatype proc proc?
  [procedure
   (vars list?)
   (body expression?)
   (saved-env env?)])

;; () -> FinalAnswer
;; Depends on: proc1, vals, cont
(define (apply-procedure)
  (cases proc proc1
    [procedure (vars body saved-env)
               (set! exp body)
               (set! env (extend-env-parallel vars vals saved-env))
               (value-of-exp)]))

;; Values
;;
;; ExpVal = Int + Bool + Proc + List[EvalVal]
;; DenVal = ExpVal

(define-datatype expval expval?
  [num-val (n number?)]
  [bool-val (b boolean?)]
  [proc-val (p proc?)]
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

(define (expval->list val)
  (cases expval val
    [list-val (l) l]
    [else (eopl:error 'expval->list "Not a list: ~s" val)]))
