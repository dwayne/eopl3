#lang racket

(provide

 initialize-input!
 getint)

(define the-input-buffer 'uninitialized)

(define (initialize-input! ints)
  (set! the-input-buffer ints))

(define (getint)
  (let ([n (car the-input-buffer)])
    (set! the-input-buffer (cdr the-input-buffer))
    n))
