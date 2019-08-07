#lang racket

(define (up lst)
  (define (prepend front back)
    (if (null? front)
        back
        (cons (car front)
              (prepend (cdr front) back))))
  (if (null? lst)
      '()
      ((if (list? (car lst)) prepend cons)
       (car lst)
       (up (cdr lst)))))

(module+ test
  (require rackunit)
  (check-equal? (up '((1 2) (3 4))) '(1 2 3 4))
  (check-equal? (up '((x (y) z))) '(x (y) z))
  (check-equal? (up '((x ()) () y (1 2 3))) '(x () y 1 2 3)))

; Notice:
;
; (down (up '((1 2)))) = '((1) (2)) ≠ '((1 2))
;
; See ex1.17.rkt for the definition of down.