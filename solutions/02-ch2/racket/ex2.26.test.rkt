#lang racket

(require "../ch1/ex1.31.rkt")
(require "./ex2.26.rkt")

(require rackunit)

(check-equal?
 (mark-leaves-with-red-depth
  (interior-node 'red
                 (interior-node 'bar
                                (leaf 26)
                                (leaf 12))
                 (interior-node 'red
                                (leaf 11)
                                (interior-node 'quux
                                               (leaf 117)
                                               (leaf 14)))))
 (a-red-blue-subtree
  (red-node
   (blue-node (list (leaf-node 1) (leaf-node 1)))
   (red-node
    (leaf-node 2)
    (blue-node (list (leaf-node 2) (leaf-node 2)))))))
