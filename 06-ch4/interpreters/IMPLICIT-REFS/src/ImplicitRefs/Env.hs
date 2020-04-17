module ImplicitRefs.Env (Env, empty, extend, apply) where

import qualified Data.List as List

data Env s v = Env [(s, v)]
-- s represents the type for identifiers
-- v represents the type for values

empty :: Env s v
empty = Env []

extend :: s -> v -> Env s v -> Env s v
extend var val (Env bindings) = Env ((var, val):bindings)

apply :: (Eq s, Show s) => Env s v -> s -> v
apply (Env bindings) var =
  applyHelper bindings var

applyHelper :: (Eq s, Show s) => [(s, v)] -> s -> v
applyHelper bindings var =
  case List.lookup var bindings of
    Nothing ->
      error ("No binding for " ++ show var)

    Just val ->
      val
