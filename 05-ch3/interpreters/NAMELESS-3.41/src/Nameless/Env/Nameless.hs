module Nameless.Env.Nameless (Env, empty, extend, apply) where

data Env v = Env [[v]]

empty :: Env v
empty = Env []

extend :: [v] -> Env v -> Env v
extend values (Env rest) = Env (values : rest)

apply :: Env v -> (Int, Int) -> v
apply (Env values) (depth, n) = listRef (listRef values depth) n

-- Helpers

listRef :: [a] -> Int -> a
listRef [] _ = error "Index out of bounds"
listRef (v:_) 0 = v
listRef (_:vs) n = listRef vs (n-1)
