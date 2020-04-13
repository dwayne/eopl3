module Nameless.Env.Static (Env, empty, extend, apply) where

import qualified Data.List as List

data Env k = Env [[k]]

empty :: Env k
empty = Env []

extend :: [k] -> Env k -> Env k
extend vars (Env rest) = Env (vars : rest)

apply :: (Eq k, Show k) => Env k -> k -> (Int, Int)
apply (Env vars) var =
  applyHelper vars var 0

applyHelper :: (Eq k, Show k) => [[k]] -> k -> Int -> (Int, Int)
applyHelper [] var _ =
  error ("No binding for " ++ show var)

applyHelper (vars:rest) var depth =
  case List.elemIndex var vars of
    Nothing ->
      applyHelper rest var (depth+1)

    Just n ->
      (depth, n)
