module Env
  ( Env
  , empty
  , extend, extendMany, extendRec

  , Found(..)
  , find
  ) where


import Data.Bifunctor (second)


data Env k v p e
  = Env [(k, Item v p e)]

data Item v p e
  = IValue v
  | IProcedure p e


empty :: Env k v p e
empty = Env []


extend :: k -> v -> Env k v p e -> Env k v p e
extend k v (Env bindings) =
  Env $ (k, IValue v) : bindings


extendMany :: [(k, v)] -> Env k v p e -> Env k v p e
extendMany kvs (Env bindings) =
  Env $ map (second IValue) kvs ++ bindings


extendRec :: k -> p -> e -> Env k v p e -> Env k v p e
extendRec k param body (Env bindings) =
  Env $ (k, IProcedure param body) : bindings


data Found k v p e
  = Value v
  | Procedure p e (Env k v p e)

find :: Eq k => k -> Env k v p e -> Maybe (Found k v p e)
find k (Env bindings) =
  findHelper k bindings

findHelper :: Eq k => k -> [(k, Item v p e)] -> Maybe (Found k v p e)
findHelper searchK bindings =
  case bindings of
    [] ->
      Nothing

    (k, item) : rest ->
      if searchK == k then
        Just $
          case item of
            IValue v ->
              Value v

            IProcedure param body ->
              Procedure param body (Env bindings)

      else
        findHelper searchK rest
