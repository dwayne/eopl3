module Env (Env, empty, extend, find) where


data Env k v
  = Env [(k, v)]


empty :: Env k v
empty = Env []


extend :: k -> v -> Env k v -> Env k v
extend k v (Env bindings) = Env $ (k, v) : bindings


find :: Eq k => k -> Env k v -> Maybe v
find k (Env bindings) = lookup k bindings
