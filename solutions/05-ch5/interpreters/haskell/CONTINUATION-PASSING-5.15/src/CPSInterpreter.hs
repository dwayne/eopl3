module CPSInterpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


import qualified Env

import Data.Bifunctor (first)
import Debug.Trace (trace)
import Parser


data Value
  = VNumber Number
  | VBool Bool
  | VProc Procedure

data Procedure
  = Procedure Id Expr Env

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
  valueOfExpr expr initEnv endCont
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

        Just (Env.Procedure param body savedEnv) ->
          applyCont cont $ Right $ VProc $ Procedure param body savedEnv

        Nothing ->
          applyCont cont $ Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (FDiff1 bExpr env : cont)

    Zero aExpr ->
      valueOfExpr aExpr env (FZero : cont)

    If condition consequent alternative ->
      valueOfExpr condition env (FIf consequent alternative env : cont)

    Let x aExpr body ->
      valueOfExpr aExpr env (FLet x body env : cont)

    Proc param body ->
      applyCont cont $ Right $ VProc $ Procedure param body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) cont

    Call rator rand ->
      valueOfExpr rator env (FRator rand env : cont)


type Cont = [Frame]

data Frame -- or Activation Record
  = FZero
  | FLet Id Expr Env
  | FIf Expr Expr Env
  | FDiff1 Expr Env
  | FDiff2 Value
  | FRator Expr Env
  | FRand Value


endCont :: Cont
endCont = []


applyCont :: Cont -> Either RuntimeError Value -> Either RuntimeError Value
applyCont cont input = do
  value <- input
  case cont of
    [] ->
      trace "End of computation" $
        Right value

    frame : nextCont ->
      case frame of
        FZero ->
          applyCont nextCont $ zero value

        FLet x body env ->
          valueOfExpr body (Env.extend x value env) nextCont

        FIf consequent alternative env ->
          computeIf value consequent alternative env nextCont

        FDiff1 bExpr env ->
          valueOfExpr bExpr env (FDiff2 value : nextCont)

        FDiff2 aValue ->
          applyCont nextCont $ diff aValue value

        FRator rand env ->
          valueOfExpr rand env (FRand value : nextCont)

        FRand ratorValue ->
          apply ratorValue value nextCont


diff :: Value -> Value -> Either RuntimeError Value
diff aValue bValue = do
  a <- toNumber aValue
  b <- toNumber bValue
  return $ VNumber $ a - b


zero :: Value -> Either RuntimeError Value
zero aValue = do
  a <- toNumber aValue
  return $ VBool $ a == 0


computeIf :: Value -> Expr -> Expr -> Env -> Cont -> Either RuntimeError Value
computeIf conditionValue consequent alternative env cont = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env cont


apply :: Value -> Value -> Cont -> Either RuntimeError Value
apply ratorValue arg cont = do
  Procedure param body savedEnv <- toProcedure ratorValue
  valueOfExpr body (Env.extend param arg savedEnv) cont


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
