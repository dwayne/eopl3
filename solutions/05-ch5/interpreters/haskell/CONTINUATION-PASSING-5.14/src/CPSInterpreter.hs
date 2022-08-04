module CPSInterpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , fact, factIter
  , run
  ) where


--
-- Exercise 5.14
--
-- Verify that the size of the largest continuation in the calculation on
-- page 150 is 3.
--
-- ghci> CPSInterpreter.run "-(-(44,11),3)"
-- The size of the largest continuation = 3
-- Right 30
--
-- fact:
--
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 1)
-- The size of the largest continuation = 4
-- Right 1
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 2)
-- The size of the largest continuation = 5
-- Right 2
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 3)
-- The size of the largest continuation = 6
-- Right 6
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 4)
-- The size of the largest continuation = 7
-- Right 24
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 5)
-- The size of the largest continuation = 8
-- Right 120
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 6)
-- The size of the largest continuation = 9
-- Right 720
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 7)
-- The size of the largest continuation = 10
-- Right 5040
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 8)
-- The size of the largest continuation = 11
-- Right 40320
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 9)
-- The size of the largest continuation = 12
-- Right 362880
-- ghci> CPSInterpreter.run (CPSInterpreter.fact 10)
-- The size of the largest continuation = 13
-- Right 3628800
--
--    n  | size of largest continuation
-- --------------------------------------
--    1  |         4
--    2  |         5
--    3  |         6
--    4  |         7
--    5  |         8
--    6  |         9
--    7  |        10
--    8  |        11
--    9  |        12
--   10  |        13
--
-- So it seems as though the size the largest continuation used by fact grows
-- linearly with n.
--
-- factIter:
--
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 1)
-- The size of the largest continuation = 3
-- Right 1
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 2)
-- The size of the largest continuation = 3
-- Right 2
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 3)
-- The size of the largest continuation = 3
-- Right 6
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 4)
-- The size of the largest continuation = 3
-- Right 24
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 5)
-- The size of the largest continuation = 3
-- Right 120
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 6)
-- The size of the largest continuation = 3
-- Right 720
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 7)
-- The size of the largest continuation = 3
-- Right 5040
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 8)
-- The size of the largest continuation = 3
-- Right 40320
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 9)
-- The size of the largest continuation = 3
-- Right 362880
-- ghci> CPSInterpreter.run (CPSInterpreter.factIter 10)
-- The size of the largest continuation = 3
-- Right 3628800
--
--    n  | size of largest continuation
-- --------------------------------------
--    1  |         3
--    2  |         3
--    3  |         3
--    4  |         3
--    5  |         3
--    6  |         3
--    7  |         3
--    8  |         3
--    9  |         3
--   10  |         3
--
-- So it seems as though the size the largest continuation used by factIter
-- is a constant.
--


import qualified Env

import Data.List (intercalate)
import Debug.Trace (trace)
import Parser


data Value
  = VNumber Number
  | VBool Bool
  | VProc Procedure

data Procedure
  = Procedure [Id] Expr Env

data Type
  = TNumber
  | TBool
  | TProc
  deriving (Eq, Show)

type Env = Env.Env Id Value Id Expr

data Error
  = SyntaxError ParseError
  | RuntimeError RuntimeError
  deriving (Eq, Show)

data RuntimeError
  = IdentifierNotFound Id
  | IncorrectNumberOfArguments Int Int
  | TypeError Type Type
  deriving (Eq, Show)


instance Eq Value where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  _ == _ = False


instance Show Value where
  show (VNumber n) = show n
  show (VBool b) = show b
  show (VProc _) = "<proc>"


fact :: Int -> String
fact n =
  "letrec                        \
  \  fact(n) =                   \
  \    if zero?(n) then          \
  \      1                       \
  \    else                      \
  \      mult(n, (fact -(n, 1))) \
  \in                            \
  \(fact " ++ show n ++ ")"


factIter :: Int -> String
factIter n =
  "letrec                                 \
  \  factiteracc(n, a) =                  \
  \    if zero?(n) then                   \
  \      a                                \
  \    else                               \
  \      (factiteracc -(n, 1) mult(n, a)) \
  \in                                     \
  \letrec                                 \
  \  factiter(n) =                        \
  \    (factiteracc n 1)                  \
  \in                                     \
  \(factiter " ++ show n ++ ")"


run :: String -> Either Error Value
run input =
  case parse input of
    Left err ->
      Left $ SyntaxError err

    Right program ->
      case valueOfProgram program of
        Right (value, largest) ->
          trace ("The size of the largest continuation = " ++ show largest) $
            Right value

        Left err ->
          Left $ RuntimeError err


valueOfProgram :: Program -> Either RuntimeError (Value, Int)
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv EndCont 1
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> Cont -> Int -> Either RuntimeError (Value, Int)
valueOfExpr expr env cont largest =
  let
    newLargest =
      max (sizeOf cont) largest
  in
  case expr of
    Const n ->
      applyCont cont newLargest $ Right $ VNumber n

    Var x ->
      applyCont cont newLargest $
      case Env.find x env of
        Just (Env.Value value) ->
          Right value

        Just (Env.Procedure params body savedEnv) ->
          Right $ VProc $ Procedure params body savedEnv

        Nothing ->
          Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (Diff1Cont bExpr env cont) newLargest

    Mult aExpr bExpr ->
      valueOfExpr aExpr env (Mult1Cont bExpr env cont) newLargest

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont cont) newLargest

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env cont) newLargest

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env cont) newLargest

    Proc params body ->
      applyCont cont newLargest $ Right $ VProc $ Procedure params body env

    Letrec name params body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name params body env) cont newLargest

    Call rator rands ->
      valueOfExpr rator env (RatorCont rands env cont) newLargest


data Cont
  = EndCont
  | ZeroCont Cont
  | LetCont Id Expr Env Cont
  | IfCont Expr Expr Env Cont
  | Diff1Cont Expr Env Cont
  | Diff2Cont Value Cont
  | Mult1Cont Expr Env Cont
  | Mult2Cont Value Cont
  | RatorCont [Expr] Env Cont
  | RandsCont Value [Expr] [Value] Env Cont
  deriving (Show)


sizeOf :: Cont -> Int
sizeOf EndCont = 1
sizeOf (ZeroCont cont) = 1 + sizeOf cont
sizeOf (LetCont _ _ _ cont) = 1 + sizeOf cont
sizeOf (IfCont _ _ _ cont) = 1 + sizeOf cont
sizeOf (Diff1Cont _ _ cont) = 1 + sizeOf cont
sizeOf (Diff2Cont _ cont) = 1 + sizeOf cont
sizeOf (Mult1Cont _ _ cont) = 1 + sizeOf cont
sizeOf (Mult2Cont _ cont) = 1 + sizeOf cont
sizeOf (RatorCont _ _ cont) = 1 + sizeOf cont
sizeOf (RandsCont _ _ _ _ cont) = 1 + sizeOf cont


applyCont :: Cont -> Int -> Either RuntimeError Value -> Either RuntimeError (Value, Int)
applyCont cont largest input = do
  value <- input
  case cont of
    EndCont ->
      Right (value, largest)

    ZeroCont nextCont ->
      applyCont nextCont largest $ zero value

    LetCont x body env nextCont ->
      valueOfExpr body (Env.extend x value env) nextCont largest

    IfCont consequent alternative env nextCont ->
      computeIf value consequent alternative env nextCont largest

    Diff1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Diff2Cont value nextCont) largest

    Diff2Cont aValue nextCont ->
      applyCont nextCont largest $ diff aValue value

    Mult1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Mult2Cont value nextCont) largest

    Mult2Cont aValue nextCont ->
      applyCont nextCont largest $ mult aValue value

    RatorCont rands env nextCont ->
      computeRands value rands [] env nextCont largest

    RandsCont ratorValue rands revArgs env nextCont ->
      computeRands ratorValue rands (value : revArgs) env nextCont largest


data Debug
  = ValueOfExpr Expr Env Cont
  | ApplyCont Cont (Either RuntimeError Value)

instance Show Debug where
  show (ValueOfExpr expr env cont) =
    intercalate "\n"
      [ "(valueOfExpr"
      , "  " ++ show expr
      , "  " ++ show env
      , "  " ++ show cont ++ ")"
      ]

  show (ApplyCont cont input) =
    intercalate "\n"
      [ "(applyCont"
      , "  " ++ show cont
      , "  " ++ show input ++ ")"
      ]


diff :: Value -> Value -> Either RuntimeError Value
diff aValue bValue = do
  a <- toNumber aValue
  b <- toNumber bValue
  return $ VNumber $ a - b


mult :: Value -> Value -> Either RuntimeError Value
mult aValue bValue = do
  a <- toNumber aValue
  b <- toNumber bValue
  return $ VNumber $ a * b


zero :: Value -> Either RuntimeError Value
zero aValue = do
  a <- toNumber aValue
  return $ VBool $ a == 0


computeIf :: Value -> Expr -> Expr -> Env -> Cont -> Int -> Either RuntimeError (Value, Int)
computeIf conditionValue consequent alternative env cont largest = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env cont largest


computeRands :: Value -> [Expr] -> [Value] -> Env -> Cont -> Int -> Either RuntimeError (Value, Int)
computeRands ratorValue rands revArgs env cont largest =
  case rands of
    [] ->
      apply ratorValue (reverse revArgs) cont largest

    rand : restRands ->
      valueOfExpr rand env (RandsCont ratorValue restRands revArgs env cont) largest


apply :: Value -> [Value] -> Cont -> Int -> Either RuntimeError (Value, Int)
apply ratorValue args cont largest = do
  Procedure params body savedEnv <- toProcedure ratorValue
  let nParams = length params
  let nArgs = length args
  if nArgs == nParams then
    valueOfExpr body (Env.extendMany (zip params args) savedEnv) cont largest
  else
    Left $ IncorrectNumberOfArguments nParams nArgs


toNumber :: Value -> Either RuntimeError Number
toNumber (VNumber n) = Right n
toNumber value = Left $ TypeError TNumber (typeOf value)


toBool :: Value -> Either RuntimeError Bool
toBool (VBool b) = Right b
toBool value = Left $ TypeError TBool (typeOf value)


toProcedure :: Value -> Either RuntimeError Procedure
toProcedure (VProc p) = Right p
toProcedure value = Left $ TypeError TProc (typeOf value)


typeOf :: Value -> Type
typeOf (VNumber _) = TNumber
typeOf (VBool _) = TBool
typeOf (VProc _) = TProc
