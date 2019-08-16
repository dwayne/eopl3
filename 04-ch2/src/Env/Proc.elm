module Env.Proc exposing
  ( Env
  , empty
  , apply
  , extend
  , isEmpty
  , hasBinding
  )


type Env k v
  = Env
      ( k -> Maybe v -- apply
      , () -> Bool -- isEmpty
      , k -> Bool -- hasBinding
      )


empty : Env k v
empty =
  Env
    ( \_ -> Nothing
    , \_ -> True
    , \_ -> False
    )


extend : k -> v -> Env k v -> Env k v
extend var value (Env (currentApply, _, currentHasBinding)) =
  Env
    ( \searchVar ->
        if searchVar == var then
          Just value
        else
          currentApply searchVar
    , \_ ->
        False
    , \searchVar ->
        searchVar == var || currentHasBinding searchVar
    )


apply : Env k v -> k -> Maybe v
apply (Env (currentApply, _, _)) var =
  currentApply var


isEmpty : Env k v -> Bool
isEmpty (Env (_, currentIsEmpty, _)) =
  currentIsEmpty ()


hasBinding : Env k v -> k -> Bool
hasBinding (Env (_, _, currentHasBinding)) var =
  currentHasBinding var
