module Lexer
  ( number, identifier

  , rBegin, rElse, rEnd, rIf, rIn, rLet, rLetrec, rProc, rSet, rThen, rZero

  , comma, equal, hyphen

  , parens
  , semiSep
  , whiteSpace
  )
  where


import qualified Data.Char as Char
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T

import AST (Id, Number)
import Control.Monad (mzero, void)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser, makeTokenParser)


number :: Parser Number
number = lexeme (read <$> P.many1 P.digit)


identifier :: Parser Id
identifier = T.identifier lexer


-- RESERVED NAMES


rBegin :: Parser ()
rBegin = reserved "begin"


rElse :: Parser ()
rElse = reserved "else"


rEnd :: Parser ()
rEnd = reserved "end"


rIf :: Parser ()
rIf = reserved "if"


rIn :: Parser ()
rIn = reserved "in"


rLet :: Parser ()
rLet = reserved "let"


rLetrec :: Parser ()
rLetrec = reserved "letrec"


rProc :: Parser ()
rProc = reserved "proc"


rSet :: Parser ()
rSet = reserved "set"


rThen :: Parser ()
rThen = reserved "then"


rZero :: Parser ()
rZero = reserved "zero?"


-- SYMBOLS


comma :: Parser ()
comma = symbol ","


equal :: Parser ()
equal = symbol "="


hyphen :: Parser ()
hyphen = symbol "-"


-- HELPERS


parens :: Parser a -> Parser a
parens = T.parens lexer


semiSep :: Parser a -> Parser [a]
semiSep = T.semiSep lexer


whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer


reserved :: String -> Parser ()
reserved = T.reserved lexer


symbol :: String -> Parser ()
symbol = void . T.symbol lexer


lexeme :: Parser a -> Parser a
lexeme = T.lexeme lexer


-- TOKEN PARSER


lexer :: TokenParser ()
lexer = makeTokenParser languageDef


languageDef :: LanguageDef ()
languageDef =
  emptyDef
    { T.identStart = identStart
    , T.identLetter = identLetter
    , T.reservedNames =
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
    , T.opStart = mzero
    , T.opLetter = mzero
    }


identStart :: Parser Char
identStart = P.satisfy Char.isAsciiLower


identLetter :: Parser Char
identLetter = P.satisfy letter
  where
    letter c = Char.isAsciiLower c || c == '?'
