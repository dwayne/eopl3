module Test.Interpreter (spec) where

import Control.Exception (evaluate)

import Test.Hspec

import qualified Nameless.Interpreter as I

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
    it "returns ()" $ do
      let input = "emptylist"

      run input `shouldBe` "()"

  describe "example 15" $ do
    it "returns (4)" $ do
      let input = "cons(4, emptylist)"

      run input `shouldBe` "(4)"

  describe "example 16" $ do
    it "returns (4 (3))" $ do
      let input = "                                 \
        \ let x = 4 in                              \
        \   cons(x,                                 \
        \        cons(cons(-(x, 1),                 \
        \                  emptylist),              \
        \             emptylist))                   "

      run input `shouldBe` "(4 (3))"

  describe "example 17" $ do
    it "returns 1" $ do
      let input = "car(cons(1, cons(2, emptylist)))"

      run input `shouldBe` "1"

  describe "example 18" $ do
    it "returns (2)" $ do
      let input = "cdr(cons(1, cons(2, emptylist)))"

      run input `shouldBe` "(2)"

  describe "example 19" $ do
    it "returns True" $ do
      let input = "null?(cdr(cdr(cons(1, cons(2, emptylist)))))"

      run input `shouldBe` "True"

  describe "example 20" $ do
    it "returns False" $ do
      let input = "null?(cdr(cons(1, cons(2, emptylist))))"

      run input `shouldBe` "False"

  describe "example 21" $ do
    it "returns 4" $ do
      let input = "                                     \
        \ let u = 7 in                                  \
        \   unpack x y = cons(u, cons(3, emptylist)) in \
        \     -(x, y)                                   "

      run input `shouldBe` "4"

  describe "example 22" $ do
    it "unpack: list is too long" $ do
      let input = "                                              \
        \ let u = 7 in                                           \
        \   unpack x y = cons(u, cons(3, cons(1, emptylist))) in \
        \     -(x, y)                                            "

      evaluate (run input) `shouldThrow` (errorCall "unpack: list is too long")

  describe "example 23" $ do
    it "unpack: list is too short" $ do
      let input = "                            \
        \ let u = 7 in                         \
        \   unpack x y = cons(u, emptylist) in \
        \     -(x, y)                          "

      evaluate (run input) `shouldThrow` (errorCall "unpack: list is too short")

run :: String -> String
run = show . I.run
