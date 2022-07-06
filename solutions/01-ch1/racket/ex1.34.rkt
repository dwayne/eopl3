#lang racket

;; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)

(define (path n bst)
  (define (walk bst ctx)
    (if (null? bst)
        ctx
        (cond
          [(< n (car bst)) (walk (cadr bst) (cons 'left ctx))]
          [(> n (car bst)) (walk (caddr bst) (cons 'right ctx))]
          [else ctx])))
  (reverse (walk bst '())))

(module+ test
  (require rackunit)

  (check-equal?
   (path 17 '(14 (7 () (12 () ()))
                 (26 (20 (17 () ())
                         ())
                     (31 () ()))))
   '(right left left)))
