module Factorial.CPSProcedural (fact) where


--
-- fact 3
-- = factHelper 3 id
-- = factHelper 2 (\val -> applyContinuation id (3 * val))
-- = factHelper 1 (\val -> applyContinuation (\val -> applyContinuation id (3 * val)) (2 * val))
-- = applyContinuation (\val -> applyContinuation (\val -> applyContinuation id (3 * val)) (2 * val)) 1
-- = (\val -> applyContinuation (\val -> applyContinuation id (3 * val)) (2 * val)) 1
-- = applyContinuation (\val -> applyContinuation id (3 * val)) (2 * 1)
-- = (\val -> applyContinuation id (3 * val)) 2
-- = applyContinuation id (3 * 2)
-- = id 6
-- = 6
--


fact :: Integer -> Integer
fact n =
    factHelper n id


factHelper :: Integer -> Continuation -> Integer
factHelper n k =
    if n <= 1 then
        applyContinuation k 1

    else
        factHelper (n - 1) (\val -> applyContinuation k (n * val))


type Continuation = Integer -> Integer


applyContinuation :: Continuation -> Integer -> Integer
applyContinuation k n =
    k n
