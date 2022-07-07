#lang racket

;; Nameless environment

(provide

 ;; Build
 empty-nenv
 extend-nenv
 extend-nenv-rec

 ;; Query
 nenv?
 apply-nenv)

(define (empty-nenv)
  '())

(define (extend-nenv vals nenv)
  (cons (cons vals #f) nenv))

(define (extend-nenv-rec bodys nenv)
  (cons (cons bodys #t) nenv))

(define (nenv? x)
  (list? x))

(define (apply-nenv nenv depth index construct-proc-val)
  (define (lookup lst i)
    (if (null? lst)
        (error 'apply-nenv "Lexical address not found: ~s ~s" depth index)
        (if (= i 0)
            lst
            (lookup (cdr lst) (- i 1)))))
  (let ([saved-nenv (lookup nenv depth)])
    (let ([vals-or-bodys (car saved-nenv)])
      (let ([val-or-body (car (lookup (car vals-or-bodys) index))])
        (if (cdr vals-or-bodys)
            ;; body
            (construct-proc-val val-or-body saved-nenv)

            ;; val
            val-or-body)))))
