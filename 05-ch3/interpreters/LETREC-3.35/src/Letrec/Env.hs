module Letrec.Env (Env, empty, extend, extendRec, apply) where

data Env s v
  = Empty
  | Bind s v (Env s v) -- name value

-- s represents the type for identifiers
-- v represents the type for values

empty :: Env s v
empty = Empty

extend :: s -> v -> Env s v -> Env s v
extend = Bind

extendRec :: s -> s -> e -> (s -> e -> Env s v -> v) -> Env s v -> Env s v
extendRec procName param body makeValue nextEnv =
  let
    env = Bind procName (makeValue param body env) nextEnv
  in
    env

apply :: (Eq s, Show s) => Env s v -> s -> v
apply env name =
  case env of
    Empty ->
      error ("No binding for " ++ show name)

    Bind varName value nextEnv ->
      if name == varName then
        value
      else
        apply nextEnv name
