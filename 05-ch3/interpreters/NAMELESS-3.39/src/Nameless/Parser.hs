module Nameless.Parser where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token

import Text.Parsec ((<|>), char, eof, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser)

import Nameless.AST.AST

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
  <|> diffExpr
  <|> zeroExpr
  <|> consExpr
  <|> carExpr
  <|> cdrExpr
  <|> nullExpr
  <|> emptyExpr
  <|> ifExpr
  <|> letExpr
  <|> procExpr
  <|> callExpr
  <|> varExpr

constExpr :: Parser Expr
constExpr = Const <$> number

diffExpr :: Parser Expr
diffExpr = minus *> (parens (Diff <$> (expr <* comma) <*> expr))
  where
    minus = char '-'

zeroExpr :: Parser Expr
zeroExpr = reserved "zero?" *> (parens (Zero <$> expr))

consExpr :: Parser Expr
consExpr = reserved "cons" *> (parens (Cons <$> (expr <* comma) <*> expr))

carExpr :: Parser Expr
carExpr = reserved "car" *> (parens (Car <$> expr))

cdrExpr :: Parser Expr
cdrExpr = reserved "cdr" *> (parens (Cdr <$> expr))

nullExpr :: Parser Expr
nullExpr = reserved "null?" *> (parens (Null <$> expr))

emptyExpr :: Parser Expr
emptyExpr = Empty <$ reserved "emptylist"

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

callExpr :: Parser Expr
callExpr =
  parens (Call <$> expr <*> expr)

varExpr :: Parser Expr
varExpr = Var <$> identifier

-- Helpers

comma :: Parser Char
comma = lexeme (char ',')

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
      [ "car"
      , "cdr"
      , "cons"
      , "else"
      , "emptylist"
      , "if"
      , "in"
      , "let"
      , "null?"
      , "proc"
      , "then"
      , "zero?"
      ]
  }
