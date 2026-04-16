module Let.Parser where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token

import Text.Parsec ((<|>), char, eof, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser)

import Let.AST
import Let.Lexer


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
  <|> varExpr
  <|> diffExpr
  <|> zeroExpr
  <|> ifExpr
  <|> letExpr


constExpr :: Parser Expr
constExpr = Const <$> number


varExpr :: Parser Expr
varExpr = Var <$> identifier


diffExpr :: Parser Expr
diffExpr = hyphen *> parens (Diff <$> expr <* comma <*> expr)


zeroExpr :: Parser Expr
zeroExpr = Zero <$ rZero <*> parens expr


ifExpr :: Parser Expr
ifExpr = If <$ rIf <*> expr <* rThen <*> expr <* rElse <*> expr


letExpr :: Parser Expr
letExpr = Let <$ rLet <*> identifier <* equal <*> expr <* rIn <*> expr
