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
    (value-of-program (parse s) init-env (end-cont))))

;; FinalAnswer = ExpVal

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
              (value-of-exp exp1 env (diff1-cont exp2 env (get-try-cont cont) cont))]

    [zero?-exp (exp1)
               (value-of-exp exp1 env (zero1-cont (get-try-cont cont) cont))]

    [cons-exp (exp1 exp2)
              (value-of-exp exp1 env (cons1-cont exp2 env (get-try-cont cont) cont))]

    [car-exp (exp1)
             (value-of-exp exp1 env (car-cont (get-try-cont cont) cont))]

    [cdr-exp (exp1)
             (value-of-exp exp1 env (cdr-cont (get-try-cont cont) cont))]

    [null?-exp (exp1)
               (value-of-exp exp1 env (null-cont (get-try-cont cont) cont))]

    [emptylist-exp ()
                   (apply-cont cont (list-val '()))]

    [list-exp (exps)
              (value-of-exps exps env cont)]

    [if-exp (exp1 exp2 exp3)
            (value-of-exp exp1 env (if-test-cont exp2 exp3 env (get-try-cont cont) cont))]

    [let-exp (vars exps body)
             (value-of-exps exps env (let-exps-cont vars body env (get-try-cont cont) cont))]

    [proc-exp (vars body)
              (apply-cont cont (proc-val (procedure vars body env)))]

    [letrec-exp (proc-name bound-var proc-body letrec-body)
                (value-of-exp
                 letrec-body
                 (extend-env-rec proc-name bound-var proc-body env)
                 cont)]

    [call-exp (rator rands)
              (value-of-exp rator env (rator-cont rands env (get-try-cont cont) cont))]

    [try-exp (exp1 var handler-exp)
             (value-of-exp exp1 env (try-cont var handler-exp env cont))]

    [raise-exp (exp1)
               (value-of-exp exp1 env (raise-cont (get-try-cont cont)))]))

(define (construct-proc-val vars body saved-env)
  (proc-val (procedure vars body saved-env)))

(define (value-of-exps exps env cont)
  (if (null? exps)
      (apply-cont cont (list-val '()))
      (value-of-exp (car exps) env (head-cont (cdr exps) env (get-try-cont cont) cont))))

;; Continuations

(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [cons1-cont
   (exp2 expression?)
   (saved-env env?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [cons2-cont
   (val1 expval?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [car-cont
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [cdr-cont
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [null-cont
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [let-exps-cont
   (vars list?)
   (body expression?)
   (saved-env env?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (saved-env env?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [diff1-cont
   (exp2 expression?)
   (saved-env env?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [diff2-cont
   (val1 expval?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [rator-cont
   (rands list?)
   (saved-env env?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [rands-cont
   (proc-val expval?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [head-cont
   (tail list?)
   (saved-env env?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [tail-cont
   (head expval?)
   (saved-try-cont maybe-continuation?)
   (saved-cont continuation?)]
  [try-cont
   (var identifier?)
   (handler-exp expression?)
   (saved-env env?)
   (saved-cont continuation?)]
  [raise-cont
   (saved-try-cont maybe-continuation?)])

(define (maybe-continuation? x)
  (or (eq? x #f) (continuation? x)))

;; get-try-cont : Cont -> #f or Cont
(define (get-try-cont cont)
  (cases continuation cont
    [end-cont ()
              #f]

    [zero1-cont (saved-try-cont saved-cont)
                saved-try-cont]

    [cons1-cont (exp2 saved-env saved-try-cont saved-cont)
                saved-try-cont]

    [cons2-cont (val1 saved-try-cont saved-cont)
                saved-try-cont]

    [car-cont (saved-try-cont saved-cont)
              saved-try-cont]

    [cdr-cont (saved-try-cont saved-cont)
              saved-try-cont]

    [null-cont (saved-try-cont saved-cont)
               saved-try-cont]

    [let-exps-cont (vars body saved-env saved-try-cont saved-cont)
                   saved-try-cont]

    [if-test-cont (exp2 exp3 saved-env saved-try-cont saved-cont)
                  saved-try-cont]

    [diff1-cont (exp2 saved-env saved-try-cont saved-cont)
                saved-try-cont]

    [diff2-cont (val1 saved-try-cont saved-cont)
                saved-try-cont]

    [rator-cont (rands saved-env saved-try-cont saved-cont)
                saved-try-cont]

    [rands-cont (proc-val saved-try-cont saved-cont)
                saved-try-cont]

    [head-cont (tail saved-env saved-try-cont saved-cont)
               saved-try-cont]

    [tail-cont (head saved-try-cont saved-cont)
               saved-try-cont]

    [try-cont (var handler-exp saved-env saved-cont)
              cont]

    [raise-cont (saved-try-cont)
                saved-try-cont]))

;; Cont x ExpVal -> FinalAnswer
(define (apply-cont cont val)
  (cases continuation cont
    [end-cont ()
              (eopl:printf "End of computation.~%")
              val]

    [zero1-cont (saved-try-cont saved-cont)
                (apply-cont saved-cont (bool-val (zero? (expval->num val))))]

    [cons1-cont (exp2 saved-env saved-try-cont saved-cont)
                (value-of-exp exp2 saved-env (cons2-cont val saved-try-cont saved-cont))]

    [cons2-cont (val1 saved-try-cont saved-cont)
                (apply-cont saved-cont (list-val (cons val1 (expval->list val))))]

    [car-cont (saved-try-cont saved-cont)
              (apply-cont saved-cont (car (expval->list val)))]

    [cdr-cont (saved-try-cont saved-cont)
              (apply-cont saved-cont (list-val (cdr (expval->list val))))]

    [null-cont (saved-try-cont saved-cont)
               (apply-cont saved-cont (bool-val (null? (expval->list val))))]

    [let-exps-cont (vars body saved-env saved-try-cont saved-cont)
                   (value-of-exp body (extend-env-parallel vars (expval->list val) saved-env) saved-cont)]

    [if-test-cont (exp2 exp3 saved-env saved-try-cont saved-cont)
                  (if (expval->bool val)
                      (value-of-exp exp2 saved-env saved-cont)
                      (value-of-exp exp3 saved-env saved-cont))]

    [diff1-cont (exp2 saved-env saved-try-cont saved-cont)
                (value-of-exp exp2 saved-env (diff2-cont val saved-try-cont saved-cont))]

    [diff2-cont (val1 saved-try-cont saved-cont)
                (apply-cont saved-cont
                            (num-val
                             (- (expval->num val1)
                                (expval->num val))))]

    [rator-cont (rands saved-env saved-try-cont saved-cont)
                (value-of-exps rands saved-env (rands-cont val saved-try-cont saved-cont))]

    [rands-cont (proc-val saved-try-cont saved-cont)
                (apply-procedure (expval->proc proc-val) (expval->list val) saved-cont)]

    [head-cont (tail saved-env saved-try-cont saved-cont)
               (value-of-exps tail saved-env (tail-cont val saved-try-cont saved-cont))]

    [tail-cont (head saved-try-cont saved-cont)
               (apply-cont saved-cont (list-val (cons head (expval->list val))))]

    [try-cont (var handler-exp saved-env saved-cont)
              (apply-cont saved-cont val)]

    [raise-cont (saved-try-cont)
                (if (eq? saved-try-cont #f)
                    (report-uncaught-exception val)
                    (apply-handler val saved-try-cont))]))


;; apply-hanlder : ExpVal -> Cont -> FinalAnswer
(define (apply-handler val cont)
  (cases continuation cont
    [try-cont (var handler-exp saved-env saved-cont)
              (value-of-exp handler-exp (extend-env var val saved-env) saved-cont)]

    [else
     (eopl:error 'apply-handler "Not a try-cont: ~s" cont)]))

(define (report-uncaught-exception exception)
  (eopl:error 'report-uncaught-exception "Uncaught exception: ~s" exception))

;; Procedure ADT

(define-datatype proc proc?
  [procedure
   (vars list?)
   (body expression?)
   (saved-env env?)])

(define (apply-procedure proc1 vals cont)
  (cases proc proc1
    [procedure (vars body saved-env)
               (value-of-exp body (extend-env-parallel vars vals saved-env) cont)]))

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
