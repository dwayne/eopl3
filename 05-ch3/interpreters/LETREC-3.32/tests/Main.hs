module Main (main) where

import Test.Hspec

import qualified Test.Interpreter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Interpreter" Test.Interpreter.spec
