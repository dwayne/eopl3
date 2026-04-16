module Letrec.Parser where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token

import Text.Parsec ((<|>), char, eof, many, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser)

import Letrec.AST
import Letrec.Lexer

parse :: String -> Program
parse input =
  case Parsec.parse program "" input of
    Left err ->
      error (show err)

    Right p ->
      p

program :: Parser Program
program = pure Program <* whiteSpace <*> expr <* eof

expr :: Parser Expr
expr
  = constExpr
  <|> diffExpr
  <|> zeroExpr
  <|> ifExpr
  <|> letrecExpr
  <|> letExpr
  <|> procExpr
  <|> callExpr
  <|> varExpr


constExpr :: Parser Expr
constExpr = Const <$> number


diffExpr :: Parser Expr
diffExpr = hyphen *> parens (pure Diff <*> expr <* comma <*> expr)


zeroExpr :: Parser Expr
zeroExpr = pure Zero <* rZero <*> parens expr


ifExpr :: Parser Expr
ifExpr = pure If <* rIf <*> expr <* rThen <*> expr <* rElse <*> expr


letrecExpr :: Parser Expr
letrecExpr = pure Letrec <* rLetrec <*> many recProc <* rIn <*> expr
  where
    recProc = pure (,,) <*> identifier <*> parens (commaSep identifier) <* equal <*> expr


letExpr :: Parser Expr
letExpr = pure Let <* rLet <*> identifier <* equal <*> expr <* rIn <*> expr


procExpr :: Parser Expr
procExpr = pure Proc <* rProc <*> parens identifier <*> expr


callExpr :: Parser Expr
callExpr = parens (Call <$> expr <*> many expr)


varExpr :: Parser Expr
varExpr = Var <$> identifier
