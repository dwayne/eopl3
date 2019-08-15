module Env.Fun exposing
  ( Env
  , empty
  , apply
  , extend
  )


type Env k v
  = Env (Maybe (k -> Maybe v))


empty : Env k v
empty =
  Env Nothing


apply : Env k v -> k -> Maybe v
apply (Env maybeF) var =
  case maybeF of
    Nothing ->
      Nothing

    Just f ->
      f var


extend : k -> v -> Env k v -> Env k v
extend var value env =
  let
    g x =
      if var == x then
        Just value
      else
        apply env x
  in
    Env (Just g)
