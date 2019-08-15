module Env.Assoc exposing
  ( Env
  , empty
  , apply
  , extend

  , isEmpty
  , hasBinding
  , extendMany
  )


type Env k v
  = Env (List (k, v))


empty : Env k v
empty =
  Env []


apply : Env k v -> k -> Maybe v
apply (Env bindings) var =
  find var bindings


find : k -> List (k, v) -> Maybe v
find var bindings =
  case bindings of
    [] ->
      Nothing

    (x, value) :: rest ->
      if var == x then
        Just value
      else
        find var rest


extend : k -> v -> Env k v -> Env k v
extend var value (Env bindings) =
  Env ((var, value) :: bindings)


-- Exercise 2.8
isEmpty : Env k v -> Bool
isEmpty (Env bindings) =
  List.isEmpty bindings


-- Exercise 2.9
hasBinding : Env k v -> k -> Bool
hasBinding (Env bindings) var =
  exists var bindings


exists : k -> List (k, v) -> Bool
exists var bindings =
  case bindings of
    [] ->
      False

    (x, value) :: rest ->
      var == x || exists var rest


-- Exercise 2.10
extendMany : List k -> List v -> Env k v -> Env k v
extendMany vars values (Env bindings) =
  Env ((zip vars values) ++ bindings)


zip : List a -> List b -> List (a, b)
zip left right =
  case (left, right) of
    ([], _) ->
      []

    (_, []) ->
      []

    (l :: restL, r :: restR) ->
      (l, r) :: zip restL restR
