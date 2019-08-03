R.T.P. that if `e ∊ LcExp`, then there are the same number of left and right
parentheses in `e`.

Recall:

```
LcExp ::= Identifier
      ::= (lambda (Identifier) LcExp)
      ::= (LcExp LcExp)
```

Proof:

In the base case, `e` is an identifier and it has 0 left and right parentheses
and so the result holds.

Assume the result holds for all substructures of `e`.

Consider `e = (lambda (id) e1)`. Since `e1` is a substructure of `e` it follows
that `e1` has the same number of left and right parentheses. Call it `p`. Then,
it's clear that `e` has `p+2` left and right parentheses. Thus, the result
holds in this case.

Consider `e = (e1 e2)`. Since `e1` and `e2` are substructures of `e` it follows
that `e1` has `p1` left and right parentheses and `e2` has `p2` left and right
parentheses. Again, it's clear that `e` has `p1+p2+1` left and right
parentheses. Thus, the result also holds in this case.

By the principle of structural induction the result holds for all `e ∊ LcExp`.

Q.E.D.
