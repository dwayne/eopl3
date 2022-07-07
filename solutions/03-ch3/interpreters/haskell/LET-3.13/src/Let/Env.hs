module Let.Env (Env, empty, extend, apply) where

data Env k v = Env [(k, v)] deriving Show

empty :: Env k v
empty = Env []

extend :: k -> v -> Env k v -> Env k v
extend k v (Env bs) = Env ((k, v) : bs)

apply :: (Eq k, Show k) => Env k v -> k -> v
apply (Env bs) k =
  case lookup k bs of
    Nothing ->
      error ("No binding for " ++ show k)

    Just v ->
      v
