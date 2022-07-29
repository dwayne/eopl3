The specification for `call-exp`:

```
     (value-of rator ρ σ_0) = (val, σ_1)
      (value-of rand ρ σ_1) = (arg, σ_2)
-------------------------------------------------
(value-of (call-exp rator rand) ρ σ_0)
= (apply-procedure (expval->proc val) arg σ_2)

(apply-procedure (procedure var body ρ) arg σ)
= (value-of body [var=arg]ρ σ)
```
