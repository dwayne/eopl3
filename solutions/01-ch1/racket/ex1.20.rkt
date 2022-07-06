#lang racket

(define (count-occurrences s slist)
  (foldl + 0 (map
              (lambda (sexp)
                (if (symbol? sexp)
                    (if (symbol=? sexp s) 1 0)
                    (count-occurrences s sexp)))
              slist)))

(module+ test
  (require rackunit)

  (check-eq?
   (count-occurrences 'x '((f x) y (((x z) x))))
   3)

  (check-eq?
   (count-occurrences 'x '((f x) y (((x z) () x))))
   3)

  (check-eq?
   (count-occurrences 'w '((f x) y (((x z) x))))
   0))