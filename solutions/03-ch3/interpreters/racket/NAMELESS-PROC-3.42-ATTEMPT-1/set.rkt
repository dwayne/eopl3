#lang racket

(provide empty-set singleton-set union-set diff-set member-of-set?)

(define (empty-set) (set))
(define (singleton-set x) (set x))
(define (union-set a b) (set-union a b))
(define (diff-set a b) (set-subtract a b))
(define (member-of-set? x s) (set-member? s x))
