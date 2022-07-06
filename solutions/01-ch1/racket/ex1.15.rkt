#lang racket

;(define (duple n x)
;  (if (zero? n)
;      '()
;      (cons x (duple (- n 1) x))))

;; (duple 3 1)
;; = (cons 1 (duple 2 1))
;; = (cons 1 (cons 1 (duple 1 1)))
;; = (cons 1 (cons 1 (cons 1 (duple 0 1))))
;; = (cons 1 (cons 1 (cons 1 '())))
;; = '(1 1 1)

(define (duple n x)
  (define (iter n acc)
    (if (zero? n)
        acc
        (iter (- n 1) (cons x acc))))
  (iter n '()))

;; (duple 3 1)
;; = (iter 3 '())
;; = (iter 2 '(1))
;; = (iter 1 '(1 1))
;; = (iter 0 '(1 1 1))
;; = '(1 1 1)

(module+ test
  (require rackunit)

  (check-equal?
   (duple 2 3)
   '(3 3))

  (check-equal?
   (duple 4 '(ha ha))
   '((ha ha) (ha ha) (ha ha) (ha ha)))

  (check-equal?
   (duple 0 '(blah))
   '()))
