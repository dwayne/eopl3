#lang eopl

(require "./env.rkt")
(require "./parser.rkt")
(require "./screen.rkt")
(require "./store.rkt")

(provide

 ;; Expressed Values
 num-val bool-val

 ;; Interpreter
 run)

(define (run s)
  (initialize-screen!)
  (initialize-store!)
  (let ([init-env (extend-env
                   'i (newref (num-val 1))
                   (extend-env
                    'v (newref (num-val 5))
                    (extend-env
                     'x (newref (num-val 10))
                     (empty-env))))])
    (value-of-program (parse s) init-env)
    (get-output)))

(define (value-of-program prog env)
  (cases program prog
    [a-program (stmt) (value-of-stmt stmt env)]))

(define (value-of-stmt stmt env)
  (cases statement stmt
    [assign-stmt (var exp)
                 (let ([val (value-of-exp exp env)]
                       [ref (apply-env env var construct-proc-val)])
                   (setref! ref val))]

    [print-stmt (exp)
                (let ([val (value-of-exp exp env)])
                  (print val))]

    [block-stmt (stmts)
                (value-of-stmts stmts env)]

    [if-stmt (exp stmt1 stmt2)
             (let ([val (value-of-exp exp)])
               (if (expval->bool val)
                   (value-of-stmt stmt1 env)
                   (value-of-stmt stmt2 env)))]

    [while-stmt (exp body)
                (value-of-while-stmt exp body env)]

    [var-stmt (vars body)
              (value-of-stmt body (bind-uninitialized-vars vars env))])
  #t)

(define (value-of-exp exp env)
  (cases expression exp
    [const-exp (n)
               (num-val n)]

    [var-exp (var)
             (deref (apply-env env var construct-proc-val))]

    [diff-exp (exp1 exp2)
              (let ([val1 (value-of-exp exp1 env)]
                    [val2 (value-of-exp exp2 env)])
                (num-val
                 (- (expval->num val1)
                    (expval->num val2))))]

    [add-exp (exp1 exp2)
             (let ([val1 (value-of-exp exp1 env)]
                   [val2 (value-of-exp exp2 env)])
               (num-val
                (+ (expval->num val1)
                   (expval->num val2))))]

    [mul-exp (exp1 exp2)
             (let ([val1 (value-of-exp exp1 env)]
                   [val2 (value-of-exp exp2 env)])
               (num-val
                (* (expval->num val1)
                   (expval->num val2))))]

    [zero?-exp (exp1)
               (let ([val1 (value-of-exp exp1 env)])
                 (if (zero? (expval->num val1))
                     (bool-val #t)
                     (bool-val #f)))]

    [not-exp (exp1)
             (let ([val1 (value-of-exp exp1 env)])
               (bool-val (not (expval->bool val1))))]

    [if-exp (exp1 exp2 exp3)
            (let ([val1 (value-of-exp exp1 env)])
              (if (expval->bool val1)
                  (value-of-exp exp2 env)
                  (value-of-exp exp3 env)))]

    [let-exp (var exp1 body)
             (let ([val1 (value-of-exp exp1 env)])
               (value-of-exp body (extend-env var (newref val1) env)))]

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

    [assign-exp (var exp1)
                (let ([val1 (value-of-exp exp1 env)]
                      [ref (apply-env env var construct-proc-val)])
                  (setref! ref val1)
                  (num-val 27))]))

(define (value-of-stmts stmts env)
  (if (null? stmts)
      #t
      (begin
        (value-of-stmt (car stmts) env)
        (value-of-stmts (cdr stmts) env))))

(define (value-of-while-stmt exp stmt env)
  (let ([val (value-of-exp exp env)])
    (if (expval->bool val)
        (begin
          (value-of-stmt stmt env)
          (value-of-while-stmt exp stmt env))
        #t)))

(define (bind-uninitialized-vars vars env)
  (if (null? vars)
      env
      (bind-uninitialized-vars
       (cdr vars)
       (extend-env (car vars) (newref (num-val 0)) env))))

(define (value-of-begin-exp exps env)
  (if (null? (cdr exps))
      (value-of-exp (car exps) env)
      (begin
        (value-of-exp (car exps) env)
        (value-of-begin-exp (cdr exps) env))))

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
               (value-of-exp body (extend-env var (newref val) saved-env))]))

;; Values
;;
;; ExpVal = Int + Bool + Proc
;; DenVal = Ref(ExpVal)

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
