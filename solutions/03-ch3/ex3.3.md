**Why is subtraction a better choice than addition for our single arithmetic
operation?**

I honestly don't know why but I'm going to guess.

Suppose we didn't want to deal with parsing negative number literals, `-n`, in
the syntax of our language. So that we can only specify natural numbers,
i.e. 0, 1, 2, etc. Then, by using subtraction we can still express negative
numbers.

```
-n = -(0, n)
```

**Thoughts**

Just because `Int ⊂ ExpVal` it doesn't mean that all `Int` values can be
entered via the syntax of the language. It just means that any `Int` can be
the value of an expression.

Notice that the only way to get a `Bool` is through the use of `zero?`. You
can't enter one directly since there is no syntax for it.

Hence, we don't need to provide syntax for all `Int`. We can if we wanted only
provide syntax for entering the natural numbers, i.e. 0, 1, 2, etc. And then
the only way to get a negative number would be through the subtraction
operation.

So my answer above does make sense.
