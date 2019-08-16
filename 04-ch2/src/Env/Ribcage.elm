module Env.Ribcage exposing
  ( Env
  , empty
  , apply
  , extend

  , extendMany
  )


type Env k v
  = Env (Ribcage k v)


type alias Ribcage k v =
  List (Ribs k v)


type alias Ribs k v =
  (List k, List v)


empty : Env k v
empty =
  Env []


apply : Env k v -> k -> Maybe v
apply (Env ribcage) var =
  find var ribcage


find : k -> Ribcage k v -> Maybe v
find var ribcage =
  case ribcage of
    [] ->
      Nothing

    ribs :: rest ->
      case findInRibs var ribs of
        Nothing ->
          find var rest

        Just value ->
          Just value


findInRibs : k -> Ribs k v -> Maybe v
findInRibs var ribs =
  case ribs of
    ([], _) ->
      Nothing

    (_, []) ->
      Nothing

    (x :: vars, value :: values) ->
      if var == x then
        Just value
      else
        findInRibs var (vars, values)


extend : k -> v -> Env k v -> Env k v
extend var value (Env ribcage) =
  Env (([var], [value]) :: ribcage)


extendMany : List k -> List v -> Env k v -> Env k v
extendMany vars values (Env ribcage) =
  Env ((vars, values) :: ribcage)
