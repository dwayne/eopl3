module ImplicitRefs.Parser where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token

import Text.Parsec ((<|>), char, eof, many, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser)

import ImplicitRefs.AST

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
  <|> beginExpr
  <|> assignExpr
  <|> ifExpr
  <|> letrecExpr
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
    comma = lexeme (char ',')

zeroExpr :: Parser Expr
zeroExpr = reserved "zero?" *> (parens (Zero <$> expr))

beginExpr :: Parser Expr
beginExpr = reserved "begin" *> (Begin <$> semiSep1 expr) <* reserved "end"

assignExpr :: Parser Expr
assignExpr = reserved "set" *> (Assign <$> (identifier <* equal) <*> expr)

ifExpr :: Parser Expr
ifExpr =
  If <$> (ifToken *> expr) <*> (thenToken *> expr) <*> (elseToken *> expr)
  where
    ifToken = reserved "if"
    thenToken = reserved "then"
    elseToken = reserved "else"

letrecExpr :: Parser Expr
letrecExpr =
  Letrec <$> (letrecToken *> many recProc) <*> (inToken *> expr)
  where
    letrecToken = reserved "letrec"
    recProc = (,,) <$> identifier <*> (parens identifier) <*> (equal *> expr)

letExpr :: Parser Expr
letExpr =
  Let <$> (letToken *> identifier) <*> (equal *> expr) <*> (inToken *> expr)
  where
    letToken = reserved "let"

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

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Token.semiSep1 lexer

inToken :: Parser ()
inToken = reserved "in"

equal :: Parser Char
equal = lexeme (char '=')

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
      [ "begin"
      , "else"
      , "end"
      , "if"
      , "in"
      , "let"
      , "letrec"
      , "proc"
      , "set"
      , "then"
      , "zero?"
      ]
  }
