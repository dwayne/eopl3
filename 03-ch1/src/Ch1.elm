module Ch1 exposing (inS)


inS : Int -> Bool
inS n =
  if n == 0 then
    True
  else if n >= 3 then
    inS (n-3)
  else
    False
