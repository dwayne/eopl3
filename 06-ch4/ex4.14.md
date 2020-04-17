The specification for `let`:

```
  (value-of exp ρ σ_0) = (val, σ_1)
------------------------------------------
(value-of (let-exp var exp body) ρ σ_0)
= (value-of body [var=l]ρ [l=val]σ_1)

where l ∉ dom(σ_1)
```
