## 1

Given,

```
                (n,k) ∊ S
---------     -------------
(0,1) ∊ S     (n+1,k+7) ∊ S
```

Then,

`S = {(0,1), (1,8), (2,15), (3,22), (4,29), (5,36), ...}
= { (n,7n+1) | n ∊ ℕ }`.

Why?

Well, `(0,1) ∊ S` by the axiom. And, by the 2nd rule it follows that
`(0+1,1+7)=(1,8) ∊ S`. Then, keep applying the 2nd rule to get the rest
of the elements in `S`.

## 2

Given,

```
               (n,k) ∊ S
---------     ------------
(0,1) ∊ S     (n+1,2k) ∊ S
```

Then,

`S = {(0,1), (1,2), (2,4), (3,8), (4,16), (5,32), ...}
= { (n,2^n) | n ∊ ℕ }`.

Why?

Well, `(0,1) ∊ S` by the axiom. And, by the 2nd rule it follows that
`(0+1,2(1))=(1,2) ∊ S`. Then, keep applying the 2nd rule to get the rest
of the elements in `S`.

## 3

Given,

```
                  (n,i,j) ∊ S
-----------     ---------------
(0,0,1) ∊ S     (n+1,j,i+j) ∊ S
```

Then,

`S = {(0,0,1), (1,1,1), (2,1,2), (3,2,3), (4,3,5), (5,5,8), ...}
= { (n,fib(n),fib(n+1)) | n ∊ ℕ }` where,

```
fib(0) = 0,
fib(1) = 1,
fib(n+2) = fib(n) + fib(n+1) for n >= 0.
```

Why?

As before we can use the axiom and the 2nd rule to generate the elements. Then,
you'd have to notice the Fibonacci sequence. The fact that it is the Fibonacci
sequence can be proved by induction on n. It's easy to see that the base case
is true. For the induction hypothesis we assume that it holds for n, i.e.
`(n,fib(n),fib(n+1)) ∊ S`. Then by rule 2 it follows that
`(n+1,fib(n+1),fib(n)+fib(n+1))=(n+1,fib(n+1),fib(n+2)) ∊ S` as required.

## 4

Given,

```
                   (n,i,j) ∊ S
-----------     -----------------
(0,1,0) ∊ S     (n+1,i+2,i+j) ∊ S
```

Then,

`S = {(0,1,0), (1,3,1), (2,5,4), (3,7,9), (4,9,16), (5,11,25), ...}
= { (n,2n+1,n^2) | n ∊ ℕ }`.

Why?

If `(n,2n+1,n^2) ∊ S` then `(n+1,(2n+1)+2,(2n+1)+(n^2))=(n+1,2(n+1)+1,(n+1)^2)
∊ S`.
