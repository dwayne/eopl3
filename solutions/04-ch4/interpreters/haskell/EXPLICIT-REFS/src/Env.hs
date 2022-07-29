module Env
  ( Env
  , empty
  , extend, extendRec

  , Found(..)
  , find
  ) where


data Env k v p e
  = Empty
  | Single k v (Env k v p e)
  | Rec [(k, p, e)] (Env k v p e)


empty :: Env k v p e
empty = Empty


extend :: k -> v -> Env k v p e -> Env k v p e
extend = Single


extendRec :: [(k, p, e)] -> Env k v p e -> Env k v p e
extendRec = Rec


data Found k v p e
  = Value v
  | Procedure p e (Env k v p e)

find :: Eq k => k -> Env k v p e -> Maybe (Found k v p e)
find searchK env =
  case env of
    Empty ->
      Nothing

    Single k v nextEnv ->
      if searchK == k then
        Just $ Value v
      else
        find searchK nextEnv

    Rec declarations nextEnv ->
      case findDeclaration searchK declarations of
        Just (p, e) ->
          Just $ Procedure p e env

        Nothing ->
          find searchK nextEnv

findDeclaration :: Eq k => k -> [(k, p, e)] -> Maybe (p, e)
findDeclaration searchK declarations =
  case declarations of
    [] ->
      Nothing

    (k, p, e) : restOfDeclarations ->
      if searchK == k then
        Just (p, e)
      else
        findDeclaration searchK restOfDeclarations
