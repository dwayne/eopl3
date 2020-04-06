module Letrec.Env (Env, empty, extend, extendRec, apply) where

data Env s v = Env (s -> v)
-- s represents the type for identifiers
-- v represents the type for values

empty :: Show s => Env s v
empty = Env (
    \name ->
      error ("No binding for " ++ show name)
  )

extend :: Eq s => s -> v -> Env s v -> Env s v
extend varName value nextEnv = Env (
    \name ->
      if name == varName then
        value
      else
        apply nextEnv name
  )

extendRec :: Eq s => s -> s -> e -> (s -> e -> Env s v -> v) -> Env s v -> Env s v
extendRec procName param body makeValue nextEnv =
  let
    env = Env (
        \name ->
          if name == procName then
            makeValue param body env
            -- N.B. We use `env`, the environment in which `procName` is defined.
            -- This is the key to making `letrec` work.
          else
            apply nextEnv name
      )
  in
    env

apply :: Env s v -> s -> v
apply (Env env) name = env name
