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

;; FinalAnswer = ExpVal

;; Program x Env x Cont -> FinalAnswer
(define (value-of-program prog env cont)
  (cases program prog
    [a-program (exp) (trampoline (value-of-exp exp env cont))]))

;; Bounce -> FinalAnswer
(define (trampoline b)
  ;; N.B. A procedural language would implement trampoline with a while loop.
  (cases bounce b
    [end-bounce (val)
                val]

    [apply-cont-bounce (cont val)
                       (trampoline (cont val))]))

;; Expression x Env x Cont -> Bounce
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
;; It uses a procedural representation.

(define (end-cont)
  (lambda (val)
    (eopl:printf "End of computation.~%")
    (end-bounce val)))

(define (zero1-cont cont)
  (lambda (val)
    ; (apply-cont cont (if (zero? (expval->num val))
    ;                      (bool-val #t)
    ;                      (bool-val #f)))
    ; This can be simplified to:
    (apply-cont cont (bool-val (zero? (expval->num val))))))

(define (let-exp-cont var body env cont)
  (lambda (val)
    (value-of-exp body (extend-env var val env) cont)))

(define (if-test-cont exp2 exp3 env cont)
  (lambda (val)
    (if (expval->bool val)
        (value-of-exp exp2 env cont)
        (value-of-exp exp3 env cont))))

(define (diff1-cont exp2 env cont)
  (lambda (val1)
    (value-of-exp exp2 env (diff2-cont val1 cont))))

(define (diff2-cont val1 cont)
  (lambda (val2)
    (apply-cont
     cont
     (num-val
      (- (expval->num val1)
         (expval->num val2))))))

(define (rator-cont rand env cont)
  (lambda (proc-val)
    (value-of-exp rand env (rand-cont proc-val cont))))

(define (rand-cont proc-val cont)
  (lambda (arg)
    (apply-procedure (expval->proc proc-val) arg cont)))

;; Cont x ExpVal -> Bounce
(define (apply-cont cont val)
  (apply-cont-bounce cont val))

;; Procedure ADT

(define-datatype proc proc?
  [procedure
   (var identifier?)
   (body expression?)
   (saved-env env?)])

;; Proc -> ExpVal -> Cont -> Bounce
(define (apply-procedure proc1 val cont)
  (cases proc proc1
    [procedure (var body saved-env)
               (value-of-exp body (extend-env var val saved-env) cont)]))

;; Bounce ADT

(define-datatype bounce bounce?
  [end-bounce
   (val expval?)]
  [apply-cont-bounce
   (cont continuation?)
   (val expval?)])

(define (continuation? x)
  #t) ;; A continuation is really a 1-argument procedure that takes an expval?.
;; FIXME: Figure out how to express that in code.

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
