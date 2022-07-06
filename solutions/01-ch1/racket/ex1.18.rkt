#lang racket

(define (swapper s1 s2 slist)
  (map
   (lambda (sexp)
     (if (symbol? sexp)
         (cond
           [(symbol=? sexp s1) s2]
           [(symbol=? sexp s2) s1]
           [else sexp])
         (swapper s1 s2 sexp)))
   slist))

(module+ test
  (require rackunit)

  (check-equal?
   (swapper 'a 'd '(a b c d))
   '(d b c a))

  (check-equal?
   (swapper 'a 'd '(a d () c d))
   '(d a () c a))

  (check-equal?
   (swapper 'x 'y '((x) y (z (x))))
   '((y) x (z (y)))))
