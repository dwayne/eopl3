module Nameless.Env.Static (Env, empty, extend, apply) where

data Env k v = Env [k]

empty :: Env k v
empty = Env []

extend :: k -> Env k v -> Env k v
extend var (Env vars) = Env (var : vars)

apply :: (Eq k, Show k, Integral v) => Env k v -> k -> v
apply (Env vars) var =
  let
    indexOf vs index =
      case vs of
        [] ->
          Nothing

        (v:rest) ->
          if var == v then
            Just index
          else
            indexOf rest (index + 1)
  in
  case indexOf vars 0 of
    Nothing ->
      error ("No binding for " ++ show var)

    Just n ->
      n
