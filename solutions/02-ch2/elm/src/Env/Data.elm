module Env.Data exposing
  ( Env
  , empty
  , apply
  , extend
  )


type Env k v
  = Empty
  | Bind k v (Env k v)


empty : Env k v
empty =
  Empty


apply : Env k v -> k -> Maybe v
apply env var =
  case env of
    Empty ->
      Nothing

    Bind x value next ->
      if var == x then
        Just value
      else
        apply next var


extend : k -> v -> Env k v -> Env k v
extend =
  Bind
