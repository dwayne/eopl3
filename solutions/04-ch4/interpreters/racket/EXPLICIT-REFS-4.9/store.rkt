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

(struct store (locations next-ref))

(define (empty-store)
  (store (make-vector 64) 0))

(define (newref val)
  (let ([next-ref (store-next-ref the-store)]
        [locations (store-locations the-store)])
    (vector-set! locations next-ref val)
    (set! the-store (struct-copy store the-store [next-ref (+ next-ref 1)]))
    next-ref))

(define (deref ref)
  (vector-ref (store-locations the-store) ref))

(define (setref! ref val)
  (vector-set! (store-locations the-store) ref val))

(define (reference? x)
  (integer? x))
