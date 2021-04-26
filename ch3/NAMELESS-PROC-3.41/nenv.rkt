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

(define (extend-nenv vals nenv)
  (cons vals nenv))

(define (nenv? x)
  (list? x))

(define (apply-nenv nenv depth position)
  (define (helper lst i)
    (if (null? lst)
        (error 'apply-nenv "Lexical address not found: ~s ~s" depth position)
        (if (= i 0)
            (find-value (car lst) position)
            (helper (cdr lst) (- i 1)))))
  (define (find-value lst i)
    (if (null? lst)
        (error 'apply-nenv "Lexical address not found: ~s ~s" depth position)
        (if (= i 0)
            (car lst)
            (find-value (cdr lst) (- i 1)))))
  (helper nenv depth))
