#lang eopl

(require "./store.rkt")

(provide

 newpair
 left
 right
 setleft
 setright

 mutpair?)

(define (mutpair? v)
  (reference? v))

;; newpair : ExpVal x ExpVal -> MutPair
(define (newpair val1 val2)
  (let ([r1 (newref val1)]
        [r2 (newref val2)])
    r1))

;; left : MutPair -> ExpVal
(define (left p)
  (deref p))

;; right : MutPair -> ExpVal
(define (right p)
  (deref (+ p 1)))

;; setleft : MutPair x ExpVal -> Unspecified
(define (setleft p val)
  (setref! p val))

;; setright : MutPair x ExpVal -> Unspecified
(define (setright p val)
   (setref! (+ p 1) val))
