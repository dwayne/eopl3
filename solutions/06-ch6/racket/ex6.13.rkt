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

  (define (endk n)
    (lambda (x)
      (printf "End of computation ~a~n" n)
      x))

  (check-equal?
   (removeall 1 '(1 2 3 4 1 2 1) (endk 1))
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
   (occurs-in? 5 '((1 2) 3 (5 4)) (endk 2.1))
   #t)
  (check-equal?
   (occurs-in? 6 '((1 2) 3 (5 4)) (endk 2.2))
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
   (remfirst 3 '(((1 2)) (((((3))))) 3 5) (endk 3))
   '(((1 2)) ((((())))) 3 5)))

;; 4
(define (depth s k)
  (if (null? s)
      (k 1)
      (if (number? (car s))
          (depth (cdr s) k)
          (depth (car s)
                 (lambda (l1)
                   (depth (cdr s)
                          (lambda (r)
                            (if (< (+ 1 l1) r)
                                (depth (cdr s) k)
                                (depth (car s)
                                       (lambda (l2)
                                         (k (+ 1 l2))))))))))))

(module+ test
  (check-equal?
   (depth '(1 (2) ((3))) (endk 4))
   3))

;; 5
(define (depth-with-let s k)
  (if (null? s)
      (k 1)
      (if (number? (car s))
          (depth-with-let (cdr s) k)
          (depth-with-let (car s)
                          (lambda (l)
                            (depth-with-let (cdr s)
                                            (lambda (r)
                                              (let ((dfirst (+ 1 l))
                                                    (drest r))
                                                (if (< dfirst drest)
                                                    (k drest)
                                                    (k dfirst))))))))))

(module+ test
  (check-equal?
   (depth-with-let '(1 (2) ((3))) (endk 5))
   3))

;; 6
(define (map f l k)
  (if (null? l)
      (k '())
      (f (car l)
         (lambda (h)
           (map f (cdr l)
                (lambda (t)
                  (k (cons h t))))))))

(module+ test
  (define (square n k)
    (k (* n n)))
  (check-equal?
   (map square '(1 2 3 4 5) (endk 6))
   '(1 4 9 16 25)))

;; 7
(define (fnlrgtn n-list n k)
  (if (null? n-list)
      (k #f)
      (if (number? (car n-list))
          (if (> (car n-list) n)
              (k (car n-list))
              (fnlrgtn (cdr n-list) n k))
          (fnlrgtn (car n-list) n
                   (lambda (first)
                     (if (number? first)
                         (k first)
                         (fnlrgtn (cdr n-list) n k)))))))

(module+ test
  (check-equal?
   (fnlrgtn '(1 (3 (2) 7 (9))) 6 (endk 7))
   7))
