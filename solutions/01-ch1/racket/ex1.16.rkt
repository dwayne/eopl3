#lang racket

;(define (invert lst)
;  (if (null? lst)
;      '()
;      (cons
;       (reverse-2-lists (car lst))
;       (invert (cdr lst)))))
;
;(define (reverse-2-lists lst)
;  (list (cadr lst) (car lst)))

(define (invert lst)
  (map reverse lst))

(module+ test
  (require rackunit)

  (check-equal?
   (invert '((a 1) (a 2) (1 b) (2 b)))
   '((1 a) (2 a) (b 1) (b 2))))
