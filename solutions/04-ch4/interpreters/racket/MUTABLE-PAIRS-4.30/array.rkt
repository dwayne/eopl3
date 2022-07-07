#lang eopl

(require "./store.rkt")

(provide

 newarray
 arrayref
 arrayset
 arraylength

 array?)

(define-datatype array array?
  [an-array
   (base reference?)
   (length number?)])

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
        (an-array ref size))
      (eopl:error 'newarray "array size must be positive: ~s" size)))

;; arrayref : Array -> Int -> ExpVal
(define (arrayref arr index)
  (cases array arr
    [an-array (base length)
              (if (in-bounds? index length)
                  (deref (+ base index))
                  (eopl:error 'arrayref "index is out of bounds: index=~s length=~s" index length))]))

;; arrayset : Array -> Int -> ExpVal -> Unspecified
(define (arrayset arr index val)
  (cases array arr
    [an-array (base length)
              (if (in-bounds? index length)
                  (setref! (+ base index) val)
                  (eopl:error 'arrayset "index is out of bounds: index=~s length=~s" index length))]))

;; arraylength : Array -> Int
(define (arraylength arr)
  (cases array arr
    [an-array (base length)
              length]))

(define (in-bounds? i n)
  (and (>= i 0) (< i n)))
