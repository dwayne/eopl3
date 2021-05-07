#lang eopl

(require "./store.rkt")

(provide

 newpair
 left
 right
 setleft
 setright

 mutpair?)

(define-datatype mutpair mutpair?
  [a-pair (left-loc reference?)
          (right-loc reference?)])

;; newpair : ExpVal x ExpVal -> MutPair
(define (newpair val1 val2)
  (a-pair (newref val1)
          (newref val2)))

;; left : MutPair -> ExpVal
(define (left p)
  (cases mutpair p
    [a-pair (left-loc right-loc)
            (deref left-loc)]))

;; right : MutPair -> ExpVal
(define (right p)
  (cases mutpair p
    [a-pair (left-loc right-loc)
            (deref right-loc)]))

;; setleft : MutPair x ExpVal -> Unspecified
(define (setleft p val)
  (cases mutpair p
    [a-pair (left-loc right-loc)
            (setref! left-loc val)]))

;; setright : MutPair x ExpVal -> Unspecified
(define (setright p val)
  (cases mutpair p
    [a-pair (left-loc right-loc)
            (setref! right-loc val)]))
