module Main (main) where

import Test.Hspec

import qualified Test.Interpreter

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Interpreter" Test.Interpreter.spec
