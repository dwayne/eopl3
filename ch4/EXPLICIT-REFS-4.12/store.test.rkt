#lang racket

(require "./store.rkt")

(require rackunit)

(let ([store (empty-store)])
  (match-let* ([(list s1 r1) (newref store 5)]
               [(list s2 r2) (newref s1 10)])
    (check-equal? (deref s2 r1) 5)
    (check-equal? (deref s2 r2) 10)

    (let ([s3 (setref! s2 r1 (deref s2 r2))])
      (let ([s4 (setref! s3 r2 15)])
        (check-equal? (deref s4 r1) 10)
        (check-equal? (deref s4 r2) 15)))))
