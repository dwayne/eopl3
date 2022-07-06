#lang racket

(require "ex1.31.rkt")

(define (mark-leaves-with-red-depth t)
  (define (mark t red-depth)
    (if (leaf? t)
        (leaf red-depth)
        (if (symbol=? (contents-of t) 'red)
            (interior-node
             (contents-of t)
             (mark (lson t) (+ red-depth 1))
             (mark (rson t) (+ red-depth 1)))
            (interior-node
             (contents-of t)
             (mark (lson t) red-depth)
             (mark (rson t) red-depth)))))
  (mark t 0))

(module+ test
  (require rackunit)

  (check-equal?
   (mark-leaves-with-red-depth
    (interior-node
     'red
     (interior-node 'bar (leaf 26) (leaf 12))
     (interior-node
      'red
      (leaf 11)
      (interior-node 'quux (leaf 117) (leaf 14)))))
   (interior-node
     'red
     (interior-node 'bar (leaf 1) (leaf 1))
     (interior-node
      'red
      (leaf 2)
      (interior-node 'quux (leaf 2) (leaf 2))))))
