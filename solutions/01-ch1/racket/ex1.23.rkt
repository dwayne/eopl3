#lang racket

(define (list-index pred lst)
  (define (iter lst n)
    (if (null? lst)
        #f
        (if (pred (car lst))
            n
            (iter (cdr lst) (+ n 1)))))
  (iter lst 0))

(module+ test
  (require rackunit)

  (check-eq?
   (list-index number? '(a 2 (1 3) b 7))
   1)

  (check-eq?
   (list-index symbol? '(a (b c) 17 foo))
   0)

  (check-false
   (list-index symbol? '(1 2 (a b) 3))))
