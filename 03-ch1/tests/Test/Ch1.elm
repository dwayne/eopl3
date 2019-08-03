module Test.Ch1 exposing (inS)


import Expect
import Fuzz
import Test exposing (Test, describe, fuzz)


import Ch1


inS : Test
inS =
  describe "inS"
    [ fuzz Fuzz.int "is True for non-negative multiples of 3 and False otherwise" <|
        \n ->
          if n >= 0 && modBy 3 n == 0 then
            Ch1.inS n
              |> Expect.true ("Expected " ++ (String.fromInt n) ++ " to be in S")
          else
            Ch1.inS n
              |> Expect.false ("Expected " ++ (String.fromInt n) ++ " not to be in S")
    ]
