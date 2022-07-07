#lang racket

(require (prefix-in bintree: "./ex2.19.rkt"))

;; Bidirectional binary trees
;;
;; Allows for moving up and down (left/right) seemlessly.
;;
;; BiBintree ::= (Bintree Listof((Direction Bintree)))
;; Direction ::= left | right

(define (number->bi-bintree n)
  (list (bintree:number->bintree n) '()))

(define (bi-bintree->bintree bt)
  (car bt))

(define (bi-bintree->ancestors bt)
  (cadr bt))

(define (at-leaf? bt)
  (bintree:at-leaf? (bi-bintree->bintree bt)))

(define (at-root? bt)
  (null? (bi-bintree->ancestors bt)))

(define (current-element bt)
  (let ([t (bi-bintree->bintree bt)])
    (if (bintree:at-leaf? t)
        (error 'current-element "At leaf")
        (bintree:bintree->number t))))

(define (move-to-left bt)
  (match bt
    [(list t ancestors)
     (if (bintree:at-leaf? t)
         (error 'move-to-left "At leaf")
         (list (bintree:bintree->left t)
               (cons (list 'left t) ancestors)))]))

(define (move-to-right bt)
  (match bt
    [(list t ancestors)
     (if (bintree:at-leaf? t)
         (error 'move-to-right "At leaf")
         (list (bintree:bintree->right t)
               (cons (list 'right t) ancestors)))]))

(define (move-up bt)
  (if (at-root? bt)
      (error 'move-up "At root")
      (match bt
        [(list t ancestors)
         (match (car ancestors)
           [(list 'left parent)
            (list (bintree:with-left parent t) (cdr ancestors))]
           [(list 'right parent)
            (list (bintree:with-right parent t) (cdr ancestors))])])))

(define (insert-to-left n bt)
  (list
   (bintree:insert-to-left n (bi-bintree->bintree bt))
   (bi-bintree->ancestors bt)))

(define (insert-to-right n bt)
  (list
   (bintree:insert-to-right n (bi-bintree->bintree bt))
   (bi-bintree->ancestors bt)))

(module+ test
  (require rackunit)

  (let ([t (insert-to-right
            14
            (insert-to-left
             12
             (number->bi-bintree 13)))])

    (check-eq?
     (current-element
      (move-to-left
       (move-to-left
        (move-up
         (insert-to-left
          15
          (move-to-left t))))))
     15)))
