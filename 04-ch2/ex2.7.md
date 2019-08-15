I used the following signature to define `apply-env`:

```elm
apply : Env k v -> k -> Maybe v
```

The only thing that can go wrong is the variable not being found in the
environment in which case `Nothing` is returned.
