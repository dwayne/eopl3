module Parser
  ( module AST
  , parse
  , ParseError
  )
  where


import qualified Text.Parsec as P

import AST
import Lexer
import Text.Parsec ((<|>), ParseError)
import Text.Parsec.String (Parser)


parse :: String -> Either ParseError Program
parse =
  P.parse program ""


program :: Parser Program
program =
  Program <$ whiteSpace <*> expr <* P.eof


expr :: Parser Expr
expr
  = constExpr
  <|> varExpr
  <|> diffExpr
  <|> zeroExpr
  <|> ifExpr
  <|> let2Expr
  <|> letExpr
  <|> procExpr
  <|> letrecExpr
  <|> callExpr


constExpr :: Parser Expr
constExpr =
  Const <$> number


varExpr :: Parser Expr
varExpr =
  Var <$> identifier


diffExpr :: Parser Expr
diffExpr =
  hyphen *> parens (Diff <$> (expr <* comma) <*> expr)


zeroExpr :: Parser Expr
zeroExpr =
  Zero <$ rZero <*> parens expr


ifExpr :: Parser Expr
ifExpr =
  If <$ rIf <*> (expr <* rThen) <*> (expr <* rElse) <*> expr


let2Expr :: Parser Expr
let2Expr =
  Let2 <$ rLet2
    <*> identifier <*> (equal *> expr)
    <*> identifier <*> (equal *> expr)
    <*> (rIn *> expr)


letExpr :: Parser Expr
letExpr =
  Let <$ rLet <*> (identifier <* equal) <*> (expr <* rIn) <*> expr


procExpr :: Parser Expr
procExpr =
  Proc <$ rProc <*> parens identifier <*> expr


letrecExpr :: Parser Expr
letrecExpr =
  Letrec
    <$ rLetrec
    <*> identifier
    <*> (parens identifier <* equal)
    <*> (expr <* rIn)
    <*> expr


callExpr :: Parser Expr
callExpr =
  parens (Call <$> expr <*> expr)