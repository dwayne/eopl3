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
  <|> letExpr
  <|> procExpr
  <|> letrecExpr
  <|> callExpr
  <|> tryExpr
  <|> raiseExpr
  <|> resumeExpr


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


tryExpr :: Parser Expr
tryExpr =
  let
    vars =
      parens $ (,) <$> identifier <*> (comma *> identifier)

    buildTry aExpr (x, c) handlerExpr =
      Try aExpr x c handlerExpr
  in
  buildTry <$ rTry <*> expr <*> (rCatch *> vars) <*> expr


raiseExpr :: Parser Expr
raiseExpr =
  Raise <$ rRaise <*> expr


resumeExpr :: Parser Expr
resumeExpr =
  rResume *> parens (Resume <$> (expr <* comma) <*> expr)
