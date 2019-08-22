R.T. give an expressed value `val ∊ ExpVal` for which `⎡⎣val⎦⎤ ≠ val`.

Take `val ∊ Bool`, for instance, `val = #t`. Then,

```
⎡⎣#t⎦⎤

=

(num-val (expval->num #t))

=

(num-val <undefined>)

=

<undefined>

≠

val
```

**N.B.** *The key step here was to realize that the set of expressed values
includes more than just numbers.*
