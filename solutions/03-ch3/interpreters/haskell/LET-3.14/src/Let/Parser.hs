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
  <|> minusExpr
  <|> addExpr
  <|> mulExpr
  <|> divExpr
  <|> ifExpr
  <|> letExpr
  <|> varExpr

boolExpr :: Parser BoolExpr
boolExpr
  = zeroExpr
  <|> equalExpr
  <|> greaterExpr
  <|> lessExpr

constExpr :: Parser Expr
constExpr = Const <$> number

diffExpr :: Parser Expr
diffExpr = minus *> (parens (Diff <$> (expr <* comma) <*> expr))
  where
    minus = char '-'

minusExpr :: Parser Expr
minusExpr = reserved "minus" *> (parens (Minus <$> expr))

addExpr :: Parser Expr
addExpr = reserved "add" *> (parens (Add <$> (expr <* comma) <*> expr))

mulExpr :: Parser Expr
mulExpr = reserved "mul" *> (parens (Mul <$> (expr <* comma) <*> expr))

divExpr :: Parser Expr
divExpr = reserved "div" *> (parens (Div <$> (expr <* comma) <*> expr))

ifExpr :: Parser Expr
ifExpr =
  If <$> (ifToken *> boolExpr) <*> (thenToken *> expr) <*> (elseToken *> expr)
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

zeroExpr :: Parser BoolExpr
zeroExpr = reserved "zero?" *> (parens (Zero <$> expr))

equalExpr :: Parser BoolExpr
equalExpr = reserved "equal?" *> (parens (Equal <$> (expr <* comma) <*> expr))

greaterExpr :: Parser BoolExpr
greaterExpr =
  reserved "greater?" *> (parens (Greater <$> (expr <* comma) <*> expr))

lessExpr :: Parser BoolExpr
lessExpr = reserved "less?" *> (parens (Less <$> (expr <* comma) <*> expr))

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
      [ "add"
      , "div"
      , "else"
      , "equal?"
      , "greater?"
      , "if"
      , "in"
      , "less?"
      , "let"
      , "minus"
      , "mul"
      , "then"
      , "zero?"
      ]
  }
