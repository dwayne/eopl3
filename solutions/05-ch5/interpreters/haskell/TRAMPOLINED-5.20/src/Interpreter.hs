module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


import qualified Env

import Data.Bifunctor (first)
import Debug.Trace (trace)
import Parser


data Bounce
  = EndBounce Value
  | ApplyContBounce Cont (Either RuntimeError Value)

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


trampoline :: Either RuntimeError Bounce -> Either RuntimeError Value
trampoline eitherBounce = do
  bounce <- eitherBounce
  case bounce of
    EndBounce value ->
      Right value

    ApplyContBounce cont input ->
      trampoline $ applyContBounce cont input


valueOfProgram :: Program -> Either RuntimeError Value
valueOfProgram (Program expr) =
  trampoline $ valueOfExpr expr initEnv EndCont
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> Cont -> Either RuntimeError Bounce
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
      valueOfExpr aExpr env (Diff1Cont bExpr env cont)

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont cont)

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env cont)

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env cont)

    Proc param body ->
      applyCont cont $ Right $ VProc $ Procedure param body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) cont

    Call rator rand ->
      valueOfExpr rator env (RatorCont rand env cont)


data Cont
  = EndCont
  | ZeroCont Cont
  | LetCont Id Expr Env Cont
  | IfCont Expr Expr Env Cont
  | Diff1Cont Expr Env Cont
  | Diff2Cont Value Cont
  | RatorCont Expr Env Cont
  | RandCont Value Cont


applyCont :: Cont -> Either RuntimeError Value -> Either RuntimeError Bounce
applyCont cont input =
  case cont of
    EndCont ->
      applyContBounce cont input

    _ ->
      return $ ApplyContBounce cont input


applyContBounce :: Cont -> Either RuntimeError Value -> Either RuntimeError Bounce
applyContBounce cont input = do
  value <- input
  case cont of
    EndCont ->
      trace "End of computation" $
        Right $ EndBounce value

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

    RatorCont rand env nextCont ->
      valueOfExpr rand env (RandCont value nextCont)

    RandCont ratorValue nextCont ->
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


computeIf :: Value -> Expr -> Expr -> Env -> Cont -> Either RuntimeError Bounce
computeIf conditionValue consequent alternative env cont = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env cont


apply :: Value -> Value -> Cont -> Either RuntimeError Bounce
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
