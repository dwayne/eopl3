#lang eopl

;; N.B. No bounds checking.

(require "./store.rkt")

(provide

 newarray
 arrayref
 arrayset

 indextoref

 array?)

(define (array? v)
  (reference? v))

;; newarray : Int x ExpVal -> Array
(define (newarray size default)
  (define (allocate n)
    (if (zero? n)
        #t
        (begin
          (newref default)
          (allocate (- n 1)))))
  (if (> size 0)
      (let ([ref (newref default)])
        (allocate (- size 1))
        ref)
      (eopl:error 'newarray "array size must be positive: ~s" size)))

;; arrayref : Array -> Int -> ExpVal
(define (arrayref array index)
  (deref (+ array index)))

;; arrayset : Array -> Int -> ExpVal -> Unspecified
(define (arrayset array index val)
  (setref! (+ array index) val))

;; indextoref : Array -> Int -> Reference
(define (indextoref array index)
  (+ array index))
