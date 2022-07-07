module Test.Interpreter (spec) where

import Test.Hspec

import qualified Proc.Interpreter as I

spec :: Spec
spec = do
  describe "example 1" $ do
    it "returns 5" $ do
      let input = "5"

      run input `shouldBe` "5"

  describe "example 2" $ do
    it "returns 10" $ do
      let input = "x"

      run input `shouldBe` "10"

  describe "example 3" $ do
    it "returns False" $ do
      let input = "zero?(i)"

      run input `shouldBe` "False"

  describe "example 4" $ do
    it "returns True" $ do
      let input = "zero?(-(i, 1))"

      run input `shouldBe` "True"

  describe "example 5" $ do
    it "returns 56" $ do
      let input = "-(55, -(x, 11))"

      run input `shouldBe` "56"

  describe "example 6" $ do
    it "returns 3" $ do
      let input = "-(-(x, 3), -(v, i))"

      run input `shouldBe` "3"

  describe "example 7" $ do
    it "returns 18" $ do
      let input = "                                          \
        \ let x = 33                                         \
        \ in let y = 22                                      \
        \    in if zero?(-(x, 11)) then -(y, 2) else -(y, 4) "

      run input `shouldBe` "18"

  describe "example 8" $ do
    it "returns 2" $ do
      let input = "let x = 5 in -(x, 3)"

      run input `shouldBe` "2"

  describe "example 9" $ do
    it "returns 3" $ do
      let input = "                       \
        \ let z = 5 in                    \
        \   let x = 3 in                  \
        \     let y = -(x, 1) in          \
        \       let x = 4 in -(z, -(x, y))"

      run input `shouldBe` "3"

  describe "example 10" $ do
    it "returns -5" $ do
      let input = "                                 \
        \ let x = 7 in                              \
        \   let y = 2 in                            \
        \     let y = let x = -(x, 1) in -(x, y) in \
        \       -(-(x, 8), y)                       "

      run input `shouldBe` "-5"

  describe "example 11" $ do
    it "returns 55" $ do
      let input = "                 \
        \ let f = proc (x) -(x, 11) \
        \ in (f (f 77))             "

      run input `shouldBe` "55"

  describe "example 12" $ do
    it "returns 55" $ do
      let input = "            \
        \ (proc (f) (f (f 77)) \
        \  proc (x) -(x, 11))  "

      run input `shouldBe` "55"

  describe "example 13" $ do
    it "returns -100" $ do
      let input = "                         \
        \ let x = 200                       \
        \ in let f = proc (z) -(z, x)       \
        \    in let x = 100                 \
        \       in let g = proc (z) -(z, x) \
        \          in -((f 1), (g 1))       "

      run input `shouldBe` "-100"

  describe "example for Exercise 3.20" $ do
    it "returns the sum of 3 and 4" $ do
      let input = "                                 \
        \ let sum = proc (x) proc (y) -(x, -(0, y)) \
        \ in ((sum 3) 4)                            "

      run input `shouldBe` "7"

  describe "example for Exercise 3.23 - timesfour" $ do
    it "returns 12" $ do
      let input = "                                           \
        \ let makemult =                                      \
        \   proc (maker)                                      \
        \     proc (x)                                        \
        \       if zero?(x) then                              \
        \         0                                           \
        \       else                                          \
        \         -(((maker maker) -(x, 1)), -(0, 4))         \
        \ in let timesfour = proc (x) ((makemult makemult) x) \
        \    in (timesfour 3)                                 \
        \                                                     "
        -- Changes:
        -- 1. "-4" is written as "-(0, 4)"
        -- 2. "times4" is written as "timesfour"

      run input `shouldBe` "12"

  describe "example for Exercise 3.23 - fact" $ do
    it "returns 5!" $ do
      let input = "                                                         \
        \  let timesmaker =                                                 \
        \    proc (maker)                                                   \
        \      proc (x)                                                     \
        \        proc (y)                                                   \
        \          if zero?(y) then                                         \
        \            0                                                      \
        \          else                                                     \
        \            -((((maker maker) x) -(y, 1)), -(0, x))                \
        \  in let times = proc (x) proc (y) (((timesmaker timesmaker) x) y) \
        \     in let factmaker =                                            \
        \          proc (maker)                                             \
        \            proc (n)                                               \
        \              if zero?(n) then                                     \
        \                1                                                  \
        \              else                                                 \
        \                ((times n) ((maker maker) -(n, 1)))                \
        \        in let fact = proc (n) ((factmaker factmaker) n)           \
        \           in (fact 5)                                             "

      run input `shouldBe` "120"

  describe "example for Exercise 3.23 - fact (alternative)" $ do
    it "returns 5!" $ do
      let input = "                                          \
        \  let timesmaker =                                  \
        \    proc (maker)                                    \
        \      proc (x)                                      \
        \        proc (y)                                    \
        \          if zero?(y) then                          \
        \            0                                       \
        \          else                                      \
        \            -((((maker maker) x) -(y, 1)), -(0, x)) \
        \  in let times = (timesmaker timesmaker)            \
        \     in let factmaker =                             \
        \          proc (maker)                              \
        \            proc (n)                                \
        \              if zero?(n) then                      \
        \                1                                   \
        \              else                                  \
        \                ((times n) ((maker maker) -(n, 1))) \
        \        in let fact = (factmaker factmaker)         \
        \           in (fact 5)                              "
        -- N.B.
        -- times = (timesmaker timesmaker)
        -- fact = (factmaker factmaker)

      run input `shouldBe` "120"

  describe "example for Exercise 3.24 - odd and even" $ do
    it "returns 1" $ do
      let input = "                                               \
        \ let evenmaker =                                         \
        \   proc (evenmaker)                                      \
        \     proc (oddmaker)                                     \
        \       proc (x)                                          \
        \         if zero?(x) then                                \
        \           1                                             \
        \         else                                            \
        \           (((oddmaker oddmaker) evenmaker) -(x, 1))     \
        \ in let oddmaker =                                       \
        \      proc (oddmaker)                                    \
        \        proc (evenmaker)                                 \
        \          proc (x)                                       \
        \            if zero?(x) then                             \
        \              0                                          \
        \            else                                         \
        \              (((evenmaker evenmaker) oddmaker) -(x, 1)) \
        \    in let odd = ((oddmaker oddmaker) evenmaker)         \
        \       in (odd 13)                                       "

      run input `shouldBe` "1"

  describe "example for Exercise 3.25 - from the book" $ do
    it "returns 12" $ do
      let input = "                               \
        \ let makerec =                           \
        \   proc (f)                              \
        \     let d =                             \
        \       proc (x)                          \
        \         proc (z) ((f (x x)) z)          \
        \     in proc (n) ((f (d d)) n)           \
        \ in let maketimes =                      \
        \      proc (f)                           \
        \        proc (x)                         \
        \          if zero?(x) then               \
        \            0                            \
        \          else                           \
        \            -((f -(x, 1)), -(0, 4))      \
        \    in let times = (makerec maketimes)   \
        \       in (times 3)                      "
        -- Changes:
        -- 1. `maketimes` is `maketimes4`
        -- 2. `times` is `times4`
        -- 3. -4 = -(0, 4)

      run input `shouldBe` "12"

  describe "example for Exercise 3.25 - my derivation" $ do
    it "returns 12" $ do
      let input = "                               \
        \ let makerec =                           \
        \   proc (f)                              \
        \     let inf =                           \
        \       proc (inf)                        \
        \         (f (inf inf))                   \
        \     in (inf inf)                        \
        \ in let maketimes =                      \
        \      proc (f)                           \
        \        proc (x)                         \
        \          if zero?(x) then               \
        \            0                            \
        \          else                           \
        \            -((f -(x, 1)), -(0, 4))      \
        \    in let times = (makerec maketimes)   \
        \       in (times 3)                      "
        -- Changes:
        -- 1. `maketimes` is `maketimes4`
        -- 2. `times` is `times4`
        -- 3. -4 = -(0, 4)

      run input `shouldBe` "12"

  describe "example for Exercise 3.37 using lexical binding" $ do
    it "returns 25" $ do
      let input = "                                           \
        \ let fact = proc (n) add1(n)                         \
        \ in let fact =                                       \
        \      proc (n)                                       \
        \        if zero?(n) then 1 else *(n, (fact -(n, 1))) \
        \    in (fact 5)                                      "

      run input `shouldBe` "25"

run :: String -> String
run = show . I.run
