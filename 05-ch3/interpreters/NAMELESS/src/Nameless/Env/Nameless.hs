module Nameless.Env.Nameless (Env, empty, extend, apply) where

data Env v = Env [v]

empty :: Env v
empty = Env []

extend :: v -> Env v -> Env v
extend v (Env values) = Env (v : values)

apply :: Env v -> Int -> v
apply (Env values) index = listRef values index

-- Helpers

listRef :: [a] -> Int -> a
listRef [] _ = error "Index out of bounds"
listRef (v:_) 0 = v
listRef (_:vs) n = listRef vs (n-1)
