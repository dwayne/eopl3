The specification for a `begin` expression:

```
     (value-of exp_1 ρ σ_0) = (val_1, σ_1)
     (value-of exp_2 ρ σ_1) = (val_2, σ_2)
                            .
                            .
                            .
   (value-of exp_n ρ σ_n-1) = (val_n, σ_n)
-------------------------------------------------
(value-of (begin-exp exp_1 exp_2 ... exp_n) ρ σ_0)
= (val_n, σ_n)
```
