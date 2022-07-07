#lang racket

(define (number->sequence current)
  (list current '() '()))

(define (current-element s)
  (car s))

(define (move-to-left s)
  (match s
    [(list current left right)
     (if (null? left)
         s
         (list (car left) (cdr left) (cons current right)))]))

(define (move-to-right s)
  (match s
    [(list current left right)
     (if (null? right)
         s
         (list (car right) (cons current left) (cdr right)))]))

(define (insert-to-left n s)
  (match s
    [(list current left right)
     (list current (cons n left) right)]))

(define (insert-to-right n s)
  (match s
    [(list current left right)
     (list current left (cons n right))]))

(define (at-left-end? s)
  (null? (cadr s)))

(define (at-right-end? s)
  (null? (caddr s)))

(module+ test
  (require rackunit)

  (check-equal?
   (number->sequence 7)
   '(7 () ()))

  (check-eq?
   (current-element '(6 (5 4 3 2 1) (7 8 9)))
   6)

  (check-equal?
   (move-to-left '(6 (5 4 3 2 1) (7 8 9)))
   '(5 (4 3 2 1) (6 7 8 9)))

  (check-equal?
   (move-to-right '(6 (5 4 3 2 1) (7 8 9)))
   '(7 (6 5 4 3 2 1) (8 9)))

  (check-equal?
   (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
   '(6 (13 5 4 3 2 1) (7 8 9)))

  (check-equal?
   (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
   '(6 (5 4 3 2 1) (13 7 8 9)))

  (check-true (at-left-end? '(1 () (2 3 4 5 6 7 8 9))))
  (check-false (at-left-end? '(2 (1) (3 4 5 6 7 8 9))))

  (check-true (at-right-end? '(9 (8 7 6 5 4 3 2 1) ())))
  (check-false (at-right-end? '(8 (7 6 5 4 3 2 1) (9)))))
