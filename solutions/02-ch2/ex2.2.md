All the representations succeed in satisfying the spec for `zero`, `is-zero?`,
and `predecessor`. Where all of them fails is with the `successor` spec.

Why?

Since no machine has unbounded memory it follows that for each representation,
there exists an `n ∊ ℕ` such that `(successor ⎡n⎤) = ⊥`. The machine runs out of
memory and cannot produce `⎡n+1⎤`.
