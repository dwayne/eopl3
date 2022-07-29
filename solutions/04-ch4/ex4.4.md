The specification for a `begin` expression:

```
[SINGLETON RULE]

(value-of (begin-exp [exp]) ρ σ)
= (value-of exp ρ σ)

[LIST RULE]

  (value-of exp ρ σ_0) = (val, σ_1); exps /= []
------------------------------------------------
(value-of (begin-exp (cons exp exps)) ρ σ_0)
= (value-of (begin-exp exps) ρ σ_1)
```

**N.B.** In the `[LIST RULE]`, since `exps /= []` (i.e. `exps` is not the empty
list), it means that `(cons exp exps)` has at least 2 elements.
