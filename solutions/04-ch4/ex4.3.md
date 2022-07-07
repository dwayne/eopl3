The specification for `call-exp`:

```
     (value-of rator ρ σ_0) = (val1, σ_1)
      (value-of rand ρ σ_1) = (val2, σ_2)
-------------------------------------------------
(value-of (call-exp rator rand) ρ σ_0)
= (apply-procedure (expval->proc val1) val2 σ_2)

(apply-procedure (procedure var body ρ) val σ)
= (value-of body [var=val]ρ σ)
```

**Q:** *Should we evaluate the operand and then the operators or vice versa?*
