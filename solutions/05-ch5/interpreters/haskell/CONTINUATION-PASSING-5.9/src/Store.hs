module Store
  ( Store, Ref
  , empty
  , newref, deref, setref
  )
  where


data Store v
  = Store [v]
  deriving (Eq, Show)


data Ref
  = Ref Int
  deriving (Eq, Show)


empty :: Store v
empty = Store []


newref :: v -> Store v -> (Ref, Store v)
newref v (Store contents) =
  let
    r =
      length contents
  in
  (Ref r, Store $ contents ++ [v])


deref :: Ref -> Store v -> Maybe v
deref (Ref r) (Store contents) =
  findByIndex r contents

findByIndex :: Int -> [v] -> Maybe v
findByIndex _ [] = Nothing
findByIndex i (x:xs)
  | i == 0 = Just x
  | otherwise = findByIndex (i-1) xs


setref :: Ref -> v -> Store v -> Maybe (Store v)
setref (Ref r) v (Store contents) =
  Store <$> setByIndex r v contents

setByIndex :: Int -> v -> [v] -> Maybe [v]
setByIndex _ _ [] = Nothing
setByIndex i v (x:xs)
  | i == 0 = Just $ v : xs
  | otherwise = ((:) x) <$> setByIndex (i-1) v xs
