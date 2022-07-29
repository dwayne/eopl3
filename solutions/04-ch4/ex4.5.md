The specification for `list`:

```
[EMPTY-LIST RULE]

(value-of (list-exp []) ρ σ)
= ((list-val []), σ)

[NON-EMPTY-LIST RULE]

              (value-of exp ρ σ_0) = (val_1, σ_1)
  (value-of (list-exp exps) ρ σ_1) = (val_2, σ_2)
--------------------------------------------------------------
(value-of (list-exp (cons exp exps)) ρ σ_0)
= ( (list-val (cons val_1
                    (expval->list val_2)))
  , σ_2)
```
