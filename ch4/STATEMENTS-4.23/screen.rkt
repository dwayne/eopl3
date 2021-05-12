#lang racket

(provide

 initialize-screen!
 print
 get-output)

(define the-screen 'uninitialized)

(define (initialize-screen!)
  (set! the-screen '()))

(define (print val)
  (set! the-screen (cons val the-screen)))

(define (get-output)
  the-screen)
