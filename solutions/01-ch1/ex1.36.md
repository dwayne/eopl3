R.T. write a procedure `g` such that `number-elements` from page 23 could be
defined as

```
(define (number-elements lst)
  (if (null? lst)
      '()
      (g (list 0 (car lst)) (number-elements (cdr lst)))))
```

Let's see what happens when `lst = '(1 2 3)`.

```
(number-elements '(1 2 3))
= (g '(0 1) (number-elements '(2 3)))
= (g '(0 1) (g '(0 2) (number-elements '(3))))
= (g '(0 1) (g '(0 2) (g '(0 3) '())))
= (g '(0 1) (g '(0 2) '((0 3))))
= (g '(0 1) '((0 2) (1 3)))
= '((0 1) (1 2) (2 3))
```

It suggests then that we should define `g` as follows:

```
(define (g p lst)
  (define (inc p)
    (cons (+ (car p) 1) (cdr p)))
  (cons p (map inc lst)))
```
