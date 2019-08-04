#lang eopl

; remove-first : Sym x Listof(Sym) -> Listof(Sym)
;
; Usage: (remove-first s los) returns a list l such that
; (car s l) is the tail of los for which the first occurrence
; of the symbol s is its head. If s does not occur in los then
; the empty list is returned.
(define (remove-first s los)
  (if (null? los)
      '()
      (if (eqv? (car los) s)
          (cdr los)
          (remove-first s (cdr los)))))