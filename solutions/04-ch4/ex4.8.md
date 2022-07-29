In `newref`:

1. It takes linear time to calculate `next-ref`.
2. It takes linear time to append `(list val)` to `the-store`.

In `deref`:

`list-ref` takes linear time to return the value associated with `ref`.

In `setref`:

`setref-inner` takes linear time to find the `ref` whose location needs to
change.
