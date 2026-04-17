module Parser where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token

import Text.Parsec ((<|>), char, eof, many, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser)

import AST.CPS_IN
import Lexer

parse :: String -> Program
parse input =
  case Parsec.parse program "" input of
    Left err ->
      error (show err)

    Right p ->
      p

program :: Parser Program
program = Program <$ whiteSpace <*> expr <* eof

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
diffExpr = hyphen *> parens (Diff <$> expr <* comma <*> expr)


zeroExpr :: Parser Expr
zeroExpr = Zero <$ rZero <*> parens expr


ifExpr :: Parser Expr
ifExpr = If <$ rIf <*> expr <* rThen <*> expr <* rElse <*> expr


letrecExpr :: Parser Expr
letrecExpr = Letrec <$ rLetrec <*> many recProc <* rIn <*> expr
  where
    recProc = (,,) <$> identifier <*> parens (commaSep identifier) <* equal <*> expr


letExpr :: Parser Expr
letExpr = Let <$ rLet <*> identifier <* equal <*> expr <* rIn <*> expr


procExpr :: Parser Expr
procExpr = Proc <$ rProc <*> parens identifier <*> expr


callExpr :: Parser Expr
callExpr = parens (Call <$> expr <*> many expr)


varExpr :: Parser Expr
varExpr = Var <$> identifier
