module Env
  ( Env
  , empty
  , extend, extendRec

  , Found(..)
  , find
  ) where


data Env k v p e
  = Env [(k, Item v p e)]

data Item v p e
  = IValue v
  | IProcedure p e


instance (Show k, Show v, Show p, Show e) => Show (Env k v p e) where
  show (Env bindings) = show bindings


instance (Show v, Show p, Show e) => Show (Item v p e) where
  show (IValue v) = show v
  show (IProcedure _ _) = "<<rec proc>>"


empty :: Env k v p e
empty = Env []


extend :: k -> v -> Env k v p e -> Env k v p e
extend k v (Env bindings) =
  Env $ (k, IValue v) : bindings


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
