#lang racket

;; Nameless environment

(provide

 ;; Build
 empty-nenv
 extend-nenv

 ;; Query
 nenv?
 apply-nenv)

(define (empty-nenv)
  '())

(define (extend-nenv val nenv)
  (cons val nenv))

(define (nenv? x)
  (list? x))

(define (apply-nenv nenv n)
  (define (helper lst i)
    (if (null? lst)
        (error 'apply-nenv "Lexical address not found: ~s" n)
        (if (= i 0)
            (car lst)
            (helper (cdr lst) (- i 1)))))
  (helper nenv n))
