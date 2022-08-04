module CPSInterpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


import qualified Env

import Data.Bifunctor (first)
import Data.List (intercalate)
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


run :: String -> Either Error Value
run input =
  case parse input of
    Left err ->
      Left $ SyntaxError err

    Right program ->
      first RuntimeError $ valueOfProgram program


valueOfProgram :: Program -> Either RuntimeError Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv EndCont
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> Cont -> Either RuntimeError Value
valueOfExpr expr env cont =
  case expr of
    Const n ->
      applyCont cont $ Right $ VNumber n

    Var x ->
      case Env.find x env of
        Just (Env.Value value) ->
          applyCont cont $ Right value

        Just (Env.Procedure params body savedEnv) ->
          applyCont cont $ Right $ VProc $ Procedure params body savedEnv

        Nothing ->
          applyCont cont $ Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (Diff1Cont bExpr env cont)

    Mult aExpr bExpr ->
      valueOfExpr aExpr env (Mult1Cont bExpr env cont)

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont cont)

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env cont)

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env cont)

    Proc params body ->
      applyCont cont $ Right $ VProc $ Procedure params body env

    Letrec name params body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name params body env) cont

    Call rator rands ->
      valueOfExpr rator env (RatorCont rands env cont)


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


applyCont :: Cont -> Either RuntimeError Value -> Either RuntimeError Value
applyCont cont input = do
  value <- input
  case cont of
    EndCont ->
      Right value

    ZeroCont nextCont ->
      applyCont nextCont $ zero value

    LetCont x body env nextCont ->
      valueOfExpr body (Env.extend x value env) nextCont

    IfCont consequent alternative env nextCont ->
      computeIf value consequent alternative env nextCont

    Diff1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Diff2Cont value nextCont)

    Diff2Cont aValue nextCont ->
      applyCont nextCont $ diff aValue value

    Mult1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Mult2Cont value nextCont)

    Mult2Cont aValue nextCont ->
      applyCont nextCont $ mult aValue value

    RatorCont rands env nextCont ->
      computeRands value rands [] env nextCont

    RandsCont ratorValue rands revArgs env nextCont ->
      computeRands ratorValue rands (value : revArgs) env nextCont


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


computeIf :: Value -> Expr -> Expr -> Env -> Cont -> Either RuntimeError Value
computeIf conditionValue consequent alternative env cont = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env cont


computeRands :: Value -> [Expr] -> [Value] -> Env -> Cont -> Either RuntimeError Value
computeRands ratorValue rands revArgs env cont =
  case rands of
    [] ->
      apply ratorValue (reverse revArgs) cont

    rand : restRands ->
      valueOfExpr rand env (RandsCont ratorValue restRands revArgs env cont)


apply :: Value -> [Value] -> Cont -> Either RuntimeError Value
apply ratorValue args cont = do
  Procedure params body savedEnv <- toProcedure ratorValue
  let nParams = length params
  let nArgs = length args
  if nArgs == nParams then
    valueOfExpr body (Env.extendMany (zip params args) savedEnv) cont
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
