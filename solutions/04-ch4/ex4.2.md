The specification for `zero?-exp`:

```
(value-of exp ρ σ_0) = (val, σ_1)
----------------------------------
(value-of (zero?-exp exp) ρ σ_0)
= ((bool-val #t), σ_1) if (expval->num val)  = 0
  ((bool-val #f), σ_1) if (expval->num val) /= 0
```
