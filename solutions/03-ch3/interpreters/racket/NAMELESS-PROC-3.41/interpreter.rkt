#lang eopl

(require "./nenv.rkt")
(require "./parser.rkt")
(require "./translator.rkt")

(provide

 ;; Expressed Values
 num-val bool-val

 ;; Interpreter
 run)

(define (run s)
  (let ([init-nenv (extend-nenv (list (num-val 1) (num-val 5) (num-val 10)) (empty-nenv))])
    (value-of-program (translate (parse s)) init-nenv)))

(define (value-of-program prog nenv)
  (cases program prog
    [a-program (exp) (value-of-exp exp nenv)]))

(define (value-of-exp exp nenv)
  (cases expression exp
    [const-exp (n)
               (num-val n)]

    [nameless-var-exp (depth position)
                      (apply-nenv nenv depth position)]

    [diff-exp (exp1 exp2)
              (let ([val1 (value-of-exp exp1 nenv)]
                    [val2 (value-of-exp exp2 nenv)])
                (num-val
                 (- (expval->num val1)
                    (expval->num val2))))]

    [zero?-exp (exp1)
               (let ([val1 (value-of-exp exp1 nenv)])
                 (if (zero? (expval->num val1))
                     (bool-val #t)
                     (bool-val #f)))]

    [if-exp (exp1 exp2 exp3)
            (let ([val1 (value-of-exp exp1 nenv)])
              (if (expval->bool val1)
                  (value-of-exp exp2 nenv)
                  (value-of-exp exp3 nenv)))]

    [nameless-let-exp (exps body)
             (let ([vals (map (lambda (exp1) (value-of-exp exp1 nenv)) exps)])
               (value-of-exp body (extend-nenv vals nenv)))]

    [nameless-proc-exp (body)
              (proc-val (procedure body nenv))]

    [call-exp (rator rands)
              (let ([proc (expval->proc (value-of-exp rator nenv))]
                    [args (map (lambda (rand) (value-of-exp rand nenv)) rands)])
                (apply-procedure proc args))]

    [else
     (eopl:error 'value-of-exp "Invalid translated expression")]))

;; Procedure ADT

(define-datatype proc proc?
  [procedure
   (body expression?)
   (saved-nenv nenv?)])

(define (apply-procedure proc1 vals)
  (cases proc proc1
    [procedure (body saved-nenv)
               (value-of-exp body (extend-nenv vals saved-nenv))]))

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
