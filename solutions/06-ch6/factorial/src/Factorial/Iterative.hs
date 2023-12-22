module Factorial.Iterative (fact) where


fact :: Integer -> Integer
fact n =
    factHelper n 1


factHelper :: Integer -> Integer -> Integer
factHelper n acc =
    if n <= 1 then
        acc

    else
        factHelper (n - 1) (acc * n)
