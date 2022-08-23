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
  <|> consExpr
  <|> carExpr
  <|> cdrExpr
  <|> nullExpr
  <|> emptyExpr
  <|> listExpr
  <|> ifExpr
  <|> letExpr
  <|> procExpr
  <|> letrecExpr
  <|> callExpr
  <|> beginExpr
  <|> assignExpr
  <|> printExpr


constExpr :: Parser Expr
constExpr =
  Const <$> number


varExpr :: Parser Expr
varExpr =
  Var <$> identifier


diffExpr :: Parser Expr
diffExpr =
  hyphen *> parens (Diff <$> expr <*> (comma *> expr))


zeroExpr :: Parser Expr
zeroExpr =
  Zero <$ rZero <*> parens expr


consExpr :: Parser Expr
consExpr =
  rCons *> parens (Cons <$> expr <*> (comma *> expr))


carExpr :: Parser Expr
carExpr =
  Car <$ rCar <*> parens expr


cdrExpr :: Parser Expr
cdrExpr =
  Cdr <$ rCdr <*> parens expr


nullExpr :: Parser Expr
nullExpr =
  Null <$ rNull <*> parens expr


emptyExpr :: Parser Expr
emptyExpr =
  Empty <$ rEmptyList


listExpr :: Parser Expr
listExpr =
  List <$ rList <*> parens (commaSep expr)


ifExpr :: Parser Expr
ifExpr =
  If <$ rIf <*> expr <*> (rThen *> expr) <*> (rElse *> expr)


letExpr :: Parser Expr
letExpr =
  Let <$ rLet <*> identifier <*> (equal *> expr) <*> (rIn *> expr)


procExpr :: Parser Expr
procExpr =
  Proc <$ rProc <*> parens identifier <*> expr


letrecExpr :: Parser Expr
letrecExpr =
  let
    declaration =
      (,,) <$> identifier <*> parens identifier <*> (equal *> expr)
  in
  Letrec <$ rLetrec <*> P.many declaration <*> (rIn *> expr)


callExpr :: Parser Expr
callExpr =
  parens (Call <$> expr <*> expr)


beginExpr :: Parser Expr
beginExpr =
  Begin <$ rBegin <*> semiSep expr <* rEnd


assignExpr :: Parser Expr
assignExpr =
  Assign <$ rSet <*> identifier <*> (equal *> expr)


printExpr :: Parser Expr
printExpr =
  Print <$ rPrint <*> parens expr
