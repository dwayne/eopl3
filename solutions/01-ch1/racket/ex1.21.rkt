#lang racket

;(define (product sos1 sos2)
;  (foldl append '() (map
;                     (lambda (s1)
;                       (map (lambda (s2) (list s1 s2)) sos2))
;                     sos1)))

;(define (product sos1 sos2)
;  (append-map
;   (lambda (s1)
;     (map (lambda (s2) (list s1 s2)) sos2))
;   sos1))

(define (product sos1 sos2)
  (for*/list ([s1 sos1]
              [s2 sos2])
    (list s1 s2)))

(module+ test
  (require rackunit)

  (check-equal?
   (list->set (product '(a b c) '(x y)))
   (list->set '((a x) (a y) (b x) (b y) (c x) (c y)))))