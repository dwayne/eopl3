module Let.Parser where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token

import Text.Parsec ((<|>), char, eof, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser)

import Let.AST

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
  <|> ifExpr
  <|> letExpr
  <|> varExpr

constExpr :: Parser Expr
constExpr = Const <$> number

diffExpr :: Parser Expr
diffExpr = minus *> (parens (Diff <$> (expr <* comma) <*> expr))
  where
    minus = char '-'
    comma = lexeme (char ',')

zeroExpr :: Parser Expr
zeroExpr = Zero <$ reserved "zero?" <*> parens expr

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
      , "then"
      , "zero?"
      ]
  }
