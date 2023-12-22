module Factorial (fact) where


fact :: Integer -> Integer
fact n =
    if n <= 1 then
        1

    else
        n * fact (n - 1)
