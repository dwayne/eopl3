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
  <|> consExpr
  <|> carExpr
  <|> cdrExpr
  <|> nullExpr
  <|> emptyListExpr
  <|> listExpr
  <|> zeroExpr
  <|> ifExpr
  <|> let3Expr
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


consExpr :: Parser Expr
consExpr =
  rCons *> parens (Cons <$> (expr <* comma) <*> expr)


carExpr :: Parser Expr
carExpr =
  Car <$ rCar <*> parens expr


cdrExpr :: Parser Expr
cdrExpr =
  Cdr <$ rCdr <*> parens expr


nullExpr :: Parser Expr
nullExpr =
  Null <$ rNull <*> parens expr


emptyListExpr :: Parser Expr
emptyListExpr =
  EmptyList <$ rEmptyList


listExpr :: Parser Expr
listExpr =
  List <$ rList <*> parens (commaSep expr)


zeroExpr :: Parser Expr
zeroExpr =
  Zero <$ rZero <*> parens expr


ifExpr :: Parser Expr
ifExpr =
  If <$ rIf <*> (expr <* rThen) <*> (expr <* rElse) <*> expr


let3Expr :: Parser Expr
let3Expr =
  Let3 <$ rLet3
    <*> identifier <*> (equal *> expr)
    <*> identifier <*> (equal *> expr)
    <*> identifier <*> (equal *> expr)
    <*> (rIn *> expr)


let2Expr :: Parser Expr
let2Expr =
  Let2 <$ rLet2
    <*> identifier <*> (equal *> expr)
    <*> identifier <*> (equal *> expr)
    <*> (rIn *> expr)


letExpr :: Parser Expr
letExpr =
  let
    bindings =
      P.many $ (,) <$> identifier <*> (equal *> expr)
  in
  Let <$ rLet <*> bindings <*> (rIn *> expr)


procExpr :: Parser Expr
procExpr =
  Proc <$ rProc <*> parens (commaSep identifier) <*> expr


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
  parens (Call <$> expr <*> P.many expr)
