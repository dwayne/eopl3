Make `setref-exp` return the old contents of the location.

```
   (value-of lhs ρ σ_0) = (val_1, σ_1)
    (expval->ref val_1) = l
   (value-of rhs ρ σ_1) = (val_2, σ_2)
------------------------------------------
(value-of (setref-exp lhs rhs) ρ σ_0)
= (σ_0(l), [l=val_2]σ_2)
```

Is this correct?

What does `(setref-exp (newref-exp (const-exp 1)) (const-exp 2))` return?

With the current specification the reference doesn't exist in `σ_0`, it only
gets created when we evaluate the `lhs`. So if we want to handle this case then
we'd need to use `σ_1(l)` instead.
