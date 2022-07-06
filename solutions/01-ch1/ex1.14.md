R.T.P. that `partial-vector-sum` is correct.

Proof:

Let `v` be any vector such that `length(v) >= 1`.

**Case 1:**

When `length(v) = 1` a direct proof suffices to show that `partial-vector-sum`
is correct.

**N.B.** *It's similar to the one given for the base case in the induction
proof below.*

**Case 2:**

Let `P(n)` be the statement `(partial-vector-sum v n) = sum(i=0..n) { v_i }`.

We will prove that `P(n)` is true for all `n`, where `0 <= n < length(v)` by
using induction on `n`.

**Base case:**

When `n=0`,

```
(partial-vector-sum v n)

= (partial-vector-sum v 0)
= (vector-ref v 0)
= v_0
= sum(i=0..0) { v_i }

sum(i=0..n) { v_i }
```

Hence, the result holds for `n=0`.

**Induction step:**

Assume the result holds for `P(k)` where `0 <= k < length(v)-1`.

**N.B.** *The condition we use for `k` requires `length(v) > 1` and that's why
we had to split the proof into two cases.*

Now,

```
(partial-vector-sum v k+1)

= (+ (vector-ref v k+1) (partial-vector-sum v k)), since k+1 is not equal to 0
= (+ (vector-ref v k+1) (sum(i=0..k) { v_k })), by the induction hypothesis
= (+ v_k+1 (sum(i=0..k) { v_k }))

sum(i=0..k+1) { v_k }
```

i.e. `P(k+1)` is true.

Hence, `P(k) => P(k+1)` is true.

Therefore, the result follows by the principle of mathematical induction.

It follows that `partial-vector-sum` is correct.

Q.E.D.
