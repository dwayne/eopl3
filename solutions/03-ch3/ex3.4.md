A derivation of figure 3.4 as a derivation tree.

```
Let ρ = [x=⎡33⎤,y=⎡22⎤].

(value-of <<-(x,11)>> ρ)
= ⎡22⎤
-------------------------------
(value-of <<zero?(-(x,11))>> ρ)
= (bool-val #f) ; since ⎣⎡22⎤⎦ /= 0
----------------------------------------------------------
(value-of <<if zero?(-(x,11)) then -(y,2) else -(y,4)>> ρ)
= (value-of <<-(y,4)>> ρ) ; since (expval->bool (bool-val #f)) == #f
= ⎡18⎤
```
