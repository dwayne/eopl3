module Nameless.Env.Static (Env, empty, extend, apply) where

import qualified Data.List as List

data Env k = Env [k]

empty :: Env k
empty = Env []

extend :: k -> Env k -> Env k
extend var (Env vars) = Env (var : vars)

apply :: (Eq k, Show k) => Env k -> k -> Int
apply (Env vars) var =
  case List.elemIndex var vars of
    Nothing ->
      error ("No binding for " ++ show var)

    Just n ->
      n
