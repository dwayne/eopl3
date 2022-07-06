#lang racket

(provide

 ;; Build
 leaf
 interior-node

 ;; Query
 leaf?

 ;; Selectors
 lson
 rson
 contents-of)

(define (leaf n) n)
(define (interior-node s l r) (list s l r))

(define (leaf? t) (exact-integer? t))

(define lson cadr)
(define rson caddr)
(define (contents-of t)
  (if (leaf? t)
      t
      (car t)))

(module+ test
  (require rackunit)

  (check-true (leaf? (leaf 1)))

  (check-equal? (contents-of (leaf 1)) 1)

  (let ([node (interior-node 'foo (leaf 1) (leaf 2))])
    (check-equal? (lson node) (leaf 1))
    (check-equal? (rson node) (leaf 2))
    (check-equal? (contents-of node) 'foo)))
