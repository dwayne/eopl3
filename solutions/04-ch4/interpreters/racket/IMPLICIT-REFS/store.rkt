#lang racket

(provide

 initialize-store!

 newref
 deref
 setref!

 reference?)

;; One store shared via a global variable.

(define the-store 'uninitialized)

(define (initialize-store!)
  (set! the-store (empty-store)))

;; Why is this needed?
;;
;; (define (get-store)
;;   the-store)

;; The main Store API
;;
;; empty-store : () -> Store
;; newref      : ExpVal -> Ref
;; deref       : Ref -> ExpVal
;; setref      : Ref x ExpVal -> Unspecified

(define (empty-store)
  '())

(define (newref val)
  (let ([next-ref (length the-store)])
    (set! the-store (append the-store (list val)))
    next-ref))

(define (deref ref)
  (list-ref the-store ref))

(define (setref! ref val)
  (define (helper lst r)
    (cond
      [(null? lst)
       (error 'invalid-reference)]
      [(zero? r)
       (cons val (cdr lst))]
      [else
       (cons (car lst) (helper (cdr lst) (- r 1)))]))
  (set! the-store (helper the-store ref)))

(define (reference? x)
  (integer? x))
