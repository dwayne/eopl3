module Factorial.CPSDataStructure (fact) where


--
-- fact 5
-- = factHelper 5 End
-- = factHelper 4 (Fact 5 End)
-- = factHelper 3 (Fact 4 (Fact 5 End))
-- = factHelper 2 (Fact 3 (Fact 4 (Fact 5 End)))
-- = factHelper 1 (Fact 2 (Fact 3 (Fact 4 (Fact 5 End))))
-- = applyContinuation (Fact 2 (Fact 3 (Fact 4 (Fact 5 End)))) 1
-- = applyContinuation (Fact 3 (Fact 4 (Fact 5 End))) (1 * 2)
-- = applyContinuation (Fact 4 (Fact 5 End)) (2 * 3)
-- = applyContinuation (Fact 5 End) (6 * 4)
-- = applyContinuation End (24 * 5)
-- = 120
--


fact :: Integer -> Integer
fact n =
    factHelper n End


factHelper :: Integer -> Continuation -> Integer
factHelper n k =
    if n <= 1 then
        applyContinuation k 1

    else
        factHelper (n - 1) (Fact n k)


data Continuation
    = End
    | Fact Integer Continuation
    deriving (Show)


applyContinuation :: Continuation -> Integer -> Integer
applyContinuation k n =
    case k of
        End ->
            n

        Fact val nextK ->
            applyContinuation nextK (n * val)
