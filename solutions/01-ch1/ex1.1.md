## 1

Let `S = {3n+2 | n ∊ ℕ} = {2, 5, 8, 11, 14, 17, ...}`.

**Top-down definition**

A natural number `n` is in `S` if and only if

1. `n = 2`, or
2. `n-3 ∊ S`.

**Bottom-up definition**

Define `S` to be the smallest set contained in ℕ that satisfies the following
two properties:

1. `2 ∊ S`, and
2. `if n ∊ S then n+3 ∊ S`.

**Rules of inference**

```
           n ∊ S
-----     -------
2 ∊ S     n+3 ∊ S
```

## 2

Let `S = {2n+3m+1 | n,m ∊ ℕ}`.

When `n=0`, `S0 = {3m+1 | m ∊ ℕ} = {1, 4, 7, 10, ...}`.

When `n=1`, `S1 = {3m+3 | m ∊ ℕ} = {3, 6, 9, 12, ...}`.

When `n=2`, `S2 = {3m+5 | m ∊ ℕ} = {5, 8, 11, 14, ...}`.

And so on, so that `S = S0 U S1 U S2 U ... = ℕ - {0, 2} = {1, 3, 5, 7, ...} U
{4, 6, 8, 10, ...}`.

**An aside:**

*Proof that `S = ℕ - {0, 2}`.*

*First let's show that 0 is not in `S` and 2 is not in `S`.*

*Suppose `0 ∊ S` then `2n+3m+1 = 0 => 2n+3m=-1` which cannot happen since the
smallest value for `2n+3m` is 0 which occurs when `n=m=0`.*

*Suppose `2 ∊ S` then `2n+3m+1 = 2 => 2n+3m=1`. Now `n` and `m` cannot both be
0 since that doesn't give us 1. So one of `n` or `m` must be non-zero. For
`n=1, m=0` we get `2=1` and for `n=0, m=1` we get `3=1` both of which are
absurd. For any other combination of `n` and `m` it also won't work since the
value increases.*

*Second we can show that every other natural number is in `S`.*

*Let `o` be an odd natural number greater than or equal to 1. Then, `o=2r+1`
for some natural number `r`. Put `n=r, m=0` to get `2(r)+3(0)+1=2r+1=o` which
is in `S` by definition. This shows that 1, 3, 5, 7, etc are all in `S`.*

*Let `e` be an even natural number greater than or equal to 4. Then, `e=2r+4`
for some natural number `r`. Put `n=r, m=1` to get `2(r)+3(1)+1=2r+4=e` which
is in `S` by definition. This shows that 4, 6, 8, 10, etc are all in `S`.*

*Hence, it follows that `S = ℕ - {0, 2}`.*

**Top-down definition**

A natural number `n` is in `S` if and only if

1. `n = 1`, `n = 4` or
2. `n-2 ∊ S`.

**Bottom-up definition**

Define `S` to be the smallest set contained in ℕ that satisfies the following
two properties:

1. `1, 4 ∊ S`, and
2. `if n ∊ S then n+2 ∊ S`.

**Rules of inference**

```
                     n ∊ S
-----     -----     -------
1 ∊ S     4 ∊ S     n+2 ∊ S
```

## 3

Let `S = {(n, 2n+1) | n ∊ ℕ} = {(0,1), (1,3), (2,5), (3,7), ...}`.

**Top-down definition**

Let `(n, m) ∊ ℕxℕ`. The pair `(n, m)` is in `S` if and only if

1. `(n, m) = (0, 1)`, or
2. `(n-1, m-2) ∊ S`.

**Bottom-up definition**

Define `S` to be the smallest set contained in `ℕxℕ` that satisfies the
following two properties:

1. `(0, 1) ∊ S`, and
2. `if (n, m) ∊ S then (n+1, m+2) ∊ S`.

**Rules of inference**

```
                 (n, m) ∊ S
----------     --------------
(0, 1) ∊ S     (n+1, m+2) ∊ S
```

## 4

Let `S = {(n, n^2) | n ∊ ℕ} = {(0,0), (1,1), (2,4), (3,9), (4,16), ...}`.

**Top-down definition**

Let `(n, m) ∊ ℕxℕ`. The pair `(n, m)` is in `S` if and only if

1. `(n, m) = (0, 0)`, or
2. `(n-1, m-2n+1) ∊ S`.

**An aside:**

*How did I work out the 2nd property?*

*From the hint we know that `(n+1)^2=n^2+2n+1`. It follows that
`n^2=(n-1)^2+2n-1`. If we imagine that `m=n^2` it follows that
`m=(n-1)^2+2n-1 => m-2n+1=(n-1)^2` which is the previous term we want.*

*For e.g. `(5,25)` is in `S` if and only if `(5-1,25-10+1)=(4,16)` is in `S` if
and only if `(4-1,16-8+1)=(3,9)` is in `S` if and only if `(3-1,9-6+1)=(2,4)`
is in `S` if and only if `(2-1,4-4+1)=(1,1)` is in `S` if and only if
`(1-1,1-2+1)=(0,0)` is in `S`, which it is by definition. And so, `(5,25)` is
in `S`.*

**Bottom-up definition**

Define `S` to be the smallest set contained in `ℕxℕ` that satisfies the
following two properties:

1. `(0, 0) ∊ S`, and
2. `if (n, m) ∊ S then (n+1, m+2n+1) ∊ S`.

**An aside:**

*How did I work out the 2nd property?*

*If `(n, m) ∊ S` then `m` has to be equal to `n^2`. By the hint we know that
`(n+1)^2=n^2+2n+1`. Therefore, `m+2n+1=n^2+2n+1=(n+1)^2` which is the next term
we want.*

*For e.g. `(0,0) ∊ S => (0+1,0+2(0)+1)=(1,1) ∊ S => (1+1,1+2(1)+1)=(2,4) ∊ S
=> (2+1,4+2(2)+1)=(3,9) ∊ S => (3+1,9+2(3)+1)=(4,16) ∊ S` and so on.*

**Rules of inference**

```
                   (n, m) ∊ S
----------     -----------------
(0, 0) ∊ S     (n+1, m+2n+1) ∊ S
```

## References

- [Mathematical operators and symbols in Unicode](https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode)
