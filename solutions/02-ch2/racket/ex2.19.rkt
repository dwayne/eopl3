#lang racket

(provide
 number->bintree
 bintree->number bintree->left bintree->right
 at-leaf?
 with-left with-right
 current-element
 move-to-left move-to-right
 insert-to-left insert-to-right)

;; Bintree ::= () | (Int Bintree Bintree)

(define (number->bintree n)
  (list n '() '()))

(define (bintree->number t)
  (car t))

(define (bintree->left t)
  (cadr t))

(define (bintree->right t)
  (caddr t))

(define (at-leaf? t)
  (null? t))

(define (with-left t left)
  (match t
    [(list n _ right)
     (list n left right)]))

(define (with-right t right)
  (match t
    [(list n left _)
     (list n left right)]))

(define (current-element t)
  (if (at-leaf? t)
      (error 'current-element "At leaf")
      (bintree->number t)))

(define (move-to-left t)
  (if (at-leaf? t)
      (error 'move-to-left "At leaf")
      (bintree->left t)))

(define (move-to-right t)
  (if (at-leaf? t)
      (error 'move-to-right "At leaf")
      (bintree->right t)))

(define (insert-to-left n t)
  (if (at-leaf? t)
      (number->bintree n)
      (list
       (bintree->number t)
       (list n (bintree->left t) '())
       (bintree->right t))))

(define (insert-to-right n t)
  (if (at-leaf? t)
      (number->bintree n)
      (list
       (bintree->number t)
       (bintree->left t)
       (list n '() (bintree->right t)))))

(module+ test
  (require rackunit)

  (check-equal?
   (number->bintree 13)
   '(13 () ()))

  (let ([t (insert-to-right
            14
            (insert-to-left
             12
             (number->bintree 13)))])
    (check-equal?
     t
     '(13
       (12 () ())
       (14 () ())))

    (check-equal?
     (move-to-left t)
     '(12 () ()))

    (check-eq?
     (current-element (move-to-left t))
     12)

    (check-true (at-leaf? (move-to-right (move-to-left t))))

    (check-equal?
     (insert-to-left 15 t)
     '(13
       (15
        (12 () ())
        ())
       (14 () ())))))
