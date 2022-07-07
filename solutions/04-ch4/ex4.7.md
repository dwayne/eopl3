Make `setref-exp` return the old contents of the location.

```
  (value-of exp1 ρ σ_0) = (l, σ_1)
  (value-of exp2 ρ σ_1) = (val, σ_2)
----------------------------------------
(value-of (setref-exp exp1 exp2) ρ σ_0)
= (σ_0((expval->ref l)), [(expval->ref l)=val]σ_2)
```
