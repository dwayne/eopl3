module Test.Interpreter (spec) where

import Test.Hspec

import qualified ImplicitRefs.Interpreter as I

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

  describe "example 14" $ do
    it "returns 12" $ do
      let input = "                                              \
        \ letrec double(x)                                       \
        \   = if zero?(x) then 0 else -((double -(x,1)), -(0,2)) \
        \ in (double 6)                                          "

      run input `shouldBe` "12"

  describe "example 15" $ do
    it "returns 1" $ do
      let input = "                                         \
        \ letrec                                            \
        \   even(x) = if zero?(x) then 1 else (odd -(x, 1)) \
        \   odd(x) = if zero?(x) then 0 else (even -(x, 1)) \
        \ in (odd 13)                                       "

      run input `shouldBe` "1"

  describe "example 16" $ do
    it "returns 1" $ do
      let input = "                                      \
        \ let x = 0                                      \
        \ in letrec                                      \
        \      even(dummy) =                             \
        \        if zero?(x) then                        \
        \          1                                     \
        \        else                                    \
        \          let dummy = set x = -(x, 1)           \
        \          in (odd 888)                          \
        \      odd(dummy) =                              \
        \        if zero?(x) then                        \
        \          0                                     \
        \        else                                    \
        \          let dummy = set x = -(x, 1)           \
        \          in (even 888)                         \
        \    in let dummy = set x = 13 in (odd 888)      "

      run input `shouldBe` "1"

  describe "example 17" $ do
    it "returns -1" $ do
      let input = "                                                            \
        \ let g = let counter = 0                                              \
        \         in proc (dummy)                                              \
        \              let dummy = set counter = -(counter, -(0, 1))           \
        \              in counter                                              \
        \ in let a = (g 11)                                                    \
        \    in let b = (g 11)                                                 \
        \       in -(a, b)                                                     "

      run input `shouldBe` "-1"

  describe "example 18" $ do
    it "returns 0" $ do
      let input = "                                                            \
        \ let g = proc (dummy)                                                 \
        \           let counter = 0                                            \
        \           in let dummy = set counter = -(counter, -(0, 1))           \
        \              in counter                                              \
        \ in let a = (g 11)                                                    \
        \    in let b = (g 11)                                                 \
        \       in -(a, b)                                                     "

      run input `shouldBe` "0"

  describe "example 19" $ do
    it "returns 0" $ do
      let input = "                    \
        \ let y = 0                    \
        \ in let x = y                 \
        \    in let dummy = set y = 11 \
        \       in x                   "
      -- Aliasing does not work since x stores a reference to a location that
      -- stores 0, i.e. x does not store a reference to y.

      run input `shouldBe` "0"

  describe "example 20" $ do
    it "returns 11" $ do
      let input = "               \
        \ let x = 22              \
        \ in let f =              \
        \      proc (z)           \
        \        let zz = -(z, x) \
        \        in zz            \
        \    in -((f 66), (f 55)) "

      run input `shouldBe` "11"

  describe "example 21" $ do
    it "returns 1" $ do
      let input = "                             \
        \ let x = 0                             \
        \ in letrec                             \
        \      even(dummy) =                    \
        \        if zero?(x) then               \
        \          1                            \
        \        else                           \
        \          begin                        \
        \            set x = -(x, 1);           \
        \            (odd 888)                  \
        \          end                          \
        \      odd(dummy) =                     \
        \        if zero?(x) then               \
        \          0                            \
        \        else                           \
        \          begin                        \
        \            set x = -(x, 1);           \
        \            (even 888)                 \
        \          end                          \
        \    in begin set x = 13; (odd 888) end "

      run input `shouldBe` "1"

  describe "example 22" $ do
    it "returns -1" $ do
      let input = "                                         \
        \ let g = let counter = 0                           \
        \         in proc (dummy)                           \
        \              begin                                \
        \                set counter = -(counter, -(0, 1)); \
        \                counter                            \
        \              end                                  \
        \ in let a = (g 11)                                 \
        \    in let b = (g 11)                              \
        \       in -(a, b)                                  "

      run input `shouldBe` "-1"

  describe "example 23" $ do
    it "returns 0" $ do
      let input = "                                        \
        \ let g = proc (dummy)                             \
        \           let counter = 0                        \
        \           in                                     \
        \             begin                                \
        \               set counter = -(counter, -(0, 1)); \
        \               counter                            \
        \             end                                  \
        \ in let a = (g 11)                                \
        \    in let b = (g 11)                             \
        \       in -(a, b)                                 "

      run input `shouldBe` "0"

  describe "example 24" $ do
    it "returns 0" $ do
      let input = "          \
        \ let y = 0          \
        \ in let x = y       \
        \    in              \
        \      begin         \
        \        set y = 11; \
        \        x           \
        \      end           "
      -- Aliasing does not work since x stores a reference to a location that
      -- stores 0, i.e. x does not store a reference to y.

      run input `shouldBe` "0"

  describe "example 25" $ do
    it "returns 11" $ do
      let input = "               \
        \ let x = 22              \
        \ in let f =              \
        \      proc (z)           \
        \        let zz = -(z, x) \
        \        in zz            \
        \    in -((f 66), (f 55)) "

      run input `shouldBe` "11"

  describe "example 26" $ do
    it "returns 12" $ do
      let input = "                    \
        \ let f =                      \
        \   proc (x) proc (y)          \
        \     begin                    \
        \       set x = -(x, -(0, 1)); \
        \       -(x, y)                \
        \     end                      \
        \ in ((f 44) 33)               "

      run input `shouldBe` "12"

run :: String -> String
run = show . I.run
