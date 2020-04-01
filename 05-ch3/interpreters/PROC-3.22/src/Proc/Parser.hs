module Proc.Parser where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token

import Text.Parsec ((<|>), char, eof, oneOf, try)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser)

import Proc.AST

parse :: String -> Program
parse input =
  case Parsec.parse program "" input of
    Left err ->
      error (show err)

    Right p ->
      p

-- Non-terminals

program :: Parser Program
program = Program <$> (whiteSpace *> expr <* eof)

expr :: Parser Expr
expr
  = constExpr
  <|> ifExpr
  <|> letExpr
  <|> procExpr
  <|> appExpr
  <|> varExpr

constExpr :: Parser Expr
constExpr = Const <$> number

ifExpr :: Parser Expr
ifExpr =
  If <$> (ifToken *> expr) <*> (thenToken *> expr) <*> (elseToken *> expr)
  where
    ifToken = reserved "if"
    thenToken = reserved "then"
    elseToken = reserved "else"

letExpr :: Parser Expr
letExpr =
  Let <$> (letToken *> identifier) <*> (equal *> expr) <*> (inToken *> expr)
  where
    letToken = reserved "let"
    inToken = reserved "in"
    equal = lexeme (char '=')

procExpr :: Parser Expr
procExpr =
  Proc <$> (procToken *> parens identifier) <*> expr
  where
    procToken = reserved "proc"

appExpr :: Parser Expr
appExpr =
  parens (diffExpr <|> zeroExpr <|> callExpr)
  where
    diffExpr = Diff <$> (minus *> expr) <*> expr
    zeroExpr = Zero <$> (reserved "zero?" *> expr)
    callExpr = Call <$> expr <*> expr
    minus = lexeme (char '-')

varExpr :: Parser Expr
varExpr = Var <$> identifier

-- Helpers

number :: Parser Number
number = lexeme (Token.decimal lexer)

identifier :: Parser Id
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

lexer :: TokenParser st
lexer = Token.makeTokenParser letDef

letDef :: LanguageDef st
letDef = emptyDef
  { Token.identStart = oneOf ['a'..'z']
  , Token.identLetter = Token.identStart letDef
  , Token.reservedNames =
      [ "else"
      , "if"
      , "in"
      , "let"
      , "proc"
      , "then"
      , "zero?"
      ]
  }
