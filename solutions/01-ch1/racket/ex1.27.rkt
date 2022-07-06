#lang racket

(define (flatten slist)
  (append-map
   (lambda (sexp)
     (if (symbol? sexp)
         (list sexp)
         (flatten sexp)))
   slist))

(module+ test
  (require rackunit)

  (check-equal?
   (flatten '(a b c))
   '(a b c))

  (check-equal?
   (flatten '((a) () (b ()) () (c)))
   '(a b c))

  (check-equal?
   (flatten '((a b) c (((d)) e)))
   '(a b c d e))

  (check-equal?
   (flatten '(a b (() (c))))
   '(a b c)))
