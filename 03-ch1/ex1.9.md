```elm
remove : a -> List a -> List a
remove s list =
  case list of
    [] ->
      list

    x :: rest ->
      if x == s then
        remove s rest
      else
        x :: remove s rest
```

**N.B.** *I implemented it in [src/Ch1.elm](src/Ch1.elm) and I tested it in
[tests/Test/Ch1.elm](tests/Test/Ch1.elm).*
