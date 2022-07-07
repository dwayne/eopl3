#lang racket

(require "./ex2.25.rkt")

(require rackunit)

(let* ([tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3))]
       [tree-2 (interior-node 'bar (leaf-node -1) tree-1)]
       [tree-3 (interior-node 'baz tree-2 (leaf-node 1))])

  (check-eq? (max-interior tree-2) 'foo)
  (check-eq? (max-interior tree-3) 'baz))
