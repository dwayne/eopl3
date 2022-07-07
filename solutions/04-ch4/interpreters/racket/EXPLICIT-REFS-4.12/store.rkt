#lang racket

(provide

 empty-store
 newref
 deref
 setref!

 store?
 reference?)

(define (empty-store)
  '())

(define (newref store val)
  (let ([next-ref (length store)])
    (list
     (append store (list val))
     next-ref)))

(define (deref store ref)
  (list-ref store ref))

(define (setref! store ref val)
  (define (helper lst r)
    (cond
      [(null? lst)
       (error 'invalid-reference)]
      [(zero? r)
       (cons val (cdr lst))]
      [else
       (cons (car lst) (helper (cdr lst) (- r 1)))]))
  (helper store ref))

(define (store? x)
  (list? x))

(define (reference? x)
  (integer? x))
