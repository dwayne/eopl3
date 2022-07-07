#lang racket

;; Nameless environment

(provide

 ;; Build
 empty-nenv
 extend-nenv
 extend-unpack-nenv

 ;; Query
 nenv?
 apply-nenv)

(define (empty-nenv)
  '())

(define (extend-nenv val nenv)
  (cons val nenv))

(define (extend-unpack-nenv vals nenv)
  (if (null? vals)
      nenv
      (extend-unpack-nenv
       (cdr vals)
       (cons (car vals) nenv))))

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
