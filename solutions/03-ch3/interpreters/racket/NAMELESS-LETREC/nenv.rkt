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

(define (extend-nenv val nenv)
  (cons (cons val #f) nenv))

(define (extend-nenv-rec body nenv)
  (cons (cons body #t) nenv))

(define (nenv? x)
  (list? x))

(define (apply-nenv nenv n construct-proc-val)
  (define (helper lst i)
    (if (null? lst)
        (error 'apply-nenv "Lexical address not found: ~s" n)
        (if (= i 0)
            (let ((result (car lst)))
              (if (cdr result)
                  (construct-proc-val (car result) lst)
                  (car result)))
            (helper (cdr lst) (- i 1)))))
  (helper nenv n))
