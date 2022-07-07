module ExplicitRefs.Store (Store, Ref, empty, newref, deref, setref) where

data Store v = Store [v]
-- v represents the type for values

type Ref = Int

empty :: Store v
empty = Store []

newref :: v -> Store v -> (Ref, Store v)
newref val (Store locations) =
  ( length locations
  , Store (locations ++ [val])
  )

deref :: Ref -> Store v -> v
deref ref (Store locations) = listRef locations ref

setref :: Ref -> v -> Store v -> Store v
setref ref val (Store locations) = Store (listSet locations ref val)

-- Helpers

listRef :: [a] -> Int -> a
listRef [] _ = error "Index out of bounds"
listRef (v:_) 0 = v
listRef (_:vs) n = listRef vs (n-1)

listSet :: [a] -> Int -> a -> [a]
listSet [] _ _ = error "Index out of bounds"
listSet (_:vs) 0 newVal = newVal:vs
listSet (v:vs) n newVal = v : (listSet vs (n-1) newVal)
