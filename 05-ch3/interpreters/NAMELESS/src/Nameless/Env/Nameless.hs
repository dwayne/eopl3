module Nameless.Env.Nameless (Env, empty, extend, apply) where

data Env k v = Env [v]

empty :: Env k v
empty = Env []

extend :: v -> Env k v -> Env k v
extend v (Env values) = Env (v : values)

apply :: (Integral k) => Env k v -> k -> v
apply (Env values) index =
  let
    listRef [] _ = error "Index out of bounds"
    listRef (v:_) 0 = v
    listRef (_:vs) n = listRef vs (n-1)
  in
  if index < 0 then
    error "Index must be greater than or equal to 0"
  else
    listRef values index
