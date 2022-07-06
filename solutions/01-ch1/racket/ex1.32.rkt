#lang racket

(require "ex1.31.rkt")

(define (double-tree t)
  (if (leaf? t)
      (* 2 (contents-of t))
      (interior-node
       (contents-of t)
       (double-tree (lson t))
       (double-tree (rson t)))))

(module+ tree
  (require rackunit)

  (check-equal?
   (double-tree (leaf 1))
   (leaf 2))

  (check-equal?
   (double-tree (interior-node 'foo (leaf 1) (leaf 2)))
   (interior-node 'foo (leaf 2) (leaf 4))))
