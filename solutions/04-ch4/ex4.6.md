Make `setref-exp` return the value of the right-hand side.

```
   (value-of lhs ρ σ_0) = (val_1, σ_1)
   (value-of rhs ρ σ_1) = (val_2, σ_2)
------------------------------------------
(value-of (setref-exp lhs rhs) ρ σ_0)
= (val_2, [(expval->ref val_1)=val_2]σ_2)
```
