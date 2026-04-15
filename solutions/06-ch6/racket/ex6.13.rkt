#lang racket

;; 1
(define (removeall n s k)
  (if (null? s)
      (k '())
      (if (number? (car s))
          (if (equal? n (car s))
              (removeall n (cdr s) k)
              (removeall n (cdr s)
                         (lambda (t)
                           (k (cons (car s) t)))))
          (removeall n (car s)
                     (lambda (h)
                       (removeall n (cdr s)
                                  (lambda (t)
                                    (k (cons h t)))))))))

(module+ test
  (require rackunit)

  (define (endk x)
    (displayln "End of computation")
    x)

  (check-equal?
   (removeall 1 '(1 2 3 4 1 2 1) endk)
   '(2 3 4 2)))

;; 2
(define (occurs-in? n s k)
  (if (null? s)
      (k #f)
      (if (number? (car s))
          (if (equal? n (car s))
              (k #t)
              (occurs-in? n (cdr s) k))
          (occurs-in? n (car s)
                      (lambda (b)
                        (if b
                            (k #t)
                            (occurs-in? n (cdr s) k)))))))

(module+ test
  (check-equal?
   (occurs-in? 5 '((1 2) 3 (5 4)) endk)
   #t)
  (check-equal?
   (occurs-in? 6 '((1 2) 3 (5 4)) endk)
   #f))

;; 3
(define (remfirst n s k)
  (define (loop s k)
    (if (null? s)
        (k '())
        (if (number? (car s))
            (if (equal? n (car s))
                (k (cdr s))
                (loop (cdr s)
                      (lambda (t)
                        (k (cons (car s) t)))))
            (occurs-in? n (car s)
                        (lambda (b)
                          (if b
                              (remfirst n (car s)
                                        (lambda (h)
                                          (k (cons h (cdr s)))))
                              (remfirst n (cdr s)
                                        (lambda (t)
                                          (k (cons (car s) t))))))))))
  (loop s k))

(module+ test
  (check-equal?
   (remfirst 3 '(((1 2)) (((((3))))) 3 5) endk)
   '(((1 2)) ((((())))) 3 5)))