module Test.LexerSpec (spec) where


import qualified Lexer
import qualified Text.Parsec as P

import Data.Either (isLeft)
import Test.Hspec
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)


spec :: Spec
spec =
  describe "lexer" $ do
    numberSpec
    identifierSpec
    reservedSpec


numberSpec :: Spec
numberSpec =
  describe "number" $ do
    it "example 1" $ do
      parse Lexer.number "0" `shouldBe` Right 0

    it "example 2" $ do
      parse Lexer.number "00" `shouldBe` Right 0

    it "example 3" $ do
      parse Lexer.number "123" `shouldBe` Right 123


identifierSpec :: Spec
identifierSpec =
  describe "identifier" $ do
    it "example 1" $ do
      parse Lexer.identifier "x" `shouldBe` Right "x"

    it "example 2" $ do
      parse Lexer.identifier "letter" `shouldBe` Right "letter"

    it "example 3" $ do
      parse Lexer.identifier "let" `shouldSatisfy` isLeft

    it "example 4" $ do
      parse Lexer.identifier "zero?" `shouldSatisfy` isLeft

    it "example 5" $ do
      parse Lexer.identifier "let2" `shouldSatisfy` isLeft


reservedSpec :: Spec
reservedSpec =
  describe "reserved" $ do
    it "example 1" $ do
      parse Lexer.rLet2 "let2" `shouldBe` Right ()


parse :: Parser a -> String -> Either ParseError a
parse p = P.parse p ""
