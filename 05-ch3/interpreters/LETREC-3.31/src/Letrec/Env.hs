module Letrec.Env (Env, empty, extend, extendRec, apply) where

data Env s v e
  = Empty
  | Bind s v (Env s v e) -- name value
  | RecBind s [s] e (Env s v e) -- name params body

-- s represents the type for identifiers
-- v represents the type for values
-- e represents the type for expressions

empty :: Env s v e
empty = Empty

extend :: s -> v -> Env s v e -> Env s v e
extend = Bind

extendRec :: s -> [s] -> e -> Env s v e -> Env s v e
extendRec = RecBind

apply :: (Eq s, Show s) => Env s v e -> s -> ([s] -> e -> Env s v e -> v) -> v
apply env name makeValue =
  case env of
    Empty ->
      error ("No binding for " ++ show name)

    Bind varName value nextEnv ->
      if name == varName then
        value
      else
        apply nextEnv name makeValue

    RecBind procName params body nextEnv ->
      if name == procName then
        makeValue params body env
        -- N.B. We use `env`, the environment in which `procName` is defined.
        -- This is the key to making `letrec` work.
      else
        apply nextEnv name makeValue
