module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


--
-- Exercise 5.35
--
-- EXCEPTIONS is inefficient because when an exception is raise, apply-handler
-- must search linearly through the continuation to find a handler.
--
-- This implementation avoids this search by making the try-cont continuation
-- available directly in each continuation.
--


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
  | UncaughtException Value
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

        Just (Env.Procedure param body savedEnv) ->
          applyCont cont $ Right $ VProc $ Procedure param body savedEnv

        Nothing ->
          applyCont cont $ Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (Diff1Cont bExpr env (getSavedTryCont cont) cont)

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont (getSavedTryCont cont) cont)

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env (getSavedTryCont cont) cont)

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env (getSavedTryCont cont) cont)

    Proc param body ->
      applyCont cont $ Right $ VProc $ Procedure param body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) cont

    Call rator rand ->
      valueOfExpr rator env (RatorCont rand env (getSavedTryCont cont) cont)

    Try aExpr x handlerExpr ->
      valueOfExpr aExpr env (TryCont x handlerExpr env cont)

    Raise aExpr ->
      valueOfExpr aExpr env (RaiseCont (getSavedTryCont cont) cont)


data Cont
  = EndCont
  | ZeroCont SavedTryCont Cont
  | LetCont Id Expr Env SavedTryCont Cont
  | IfCont Expr Expr Env SavedTryCont Cont
  | Diff1Cont Expr Env SavedTryCont Cont
  | Diff2Cont Value SavedTryCont Cont
  | RatorCont Expr Env SavedTryCont Cont
  | RandCont Value SavedTryCont Cont
  | TryCont Id Expr Env Cont
  | RaiseCont SavedTryCont Cont


data SavedTryCont
  = NotSaved
  | SavedTryCont Id Expr Env Cont


applyCont :: Cont -> Either RuntimeError Value -> Either RuntimeError Value
applyCont cont input = do
  value <- input
  case cont of
    EndCont ->
      trace "End of computation" $
        Right value

    ZeroCont _ nextCont ->
      applyCont nextCont $ zero value

    LetCont x body env _ nextCont ->
      valueOfExpr body (Env.extend x value env) nextCont

    IfCont consequent alternative env _ nextCont ->
      computeIf value consequent alternative env nextCont

    Diff1Cont bExpr env _ nextCont ->
      valueOfExpr bExpr env (Diff2Cont value (getSavedTryCont nextCont) nextCont)

    Diff2Cont aValue _ nextCont ->
      applyCont nextCont $ diff aValue value

    RatorCont rand env _ nextCont ->
      valueOfExpr rand env (RandCont value (getSavedTryCont nextCont) nextCont)

    RandCont ratorValue _ nextCont ->
      apply ratorValue value nextCont

    TryCont _ _ _ nextCont ->
      applyCont nextCont input

    RaiseCont savedTryCont _ ->
      applyHandler savedTryCont value


getSavedTryCont :: Cont -> SavedTryCont
getSavedTryCont cont =
  case cont of
    EndCont ->
      NotSaved

    ZeroCont savedTryCont _ ->
      savedTryCont

    LetCont _ _ _ savedTryCont _ ->
      savedTryCont

    IfCont _ _ _ savedTryCont _ ->
      savedTryCont

    Diff1Cont _ _ savedTryCont _ ->
      savedTryCont

    Diff2Cont _ savedTryCont _ ->
      savedTryCont

    RatorCont _ _ savedTryCont _ ->
      savedTryCont

    RandCont _ savedTryCont _ ->
      savedTryCont

    TryCont x handlerExpr env nextCont ->
      SavedTryCont x handlerExpr env nextCont

    RaiseCont savedTryCont _ ->
      savedTryCont


applyHandler :: SavedTryCont -> Value -> Either RuntimeError Value
applyHandler savedTryCont value =
  case savedTryCont of
    SavedTryCont x handlerExpr savedEnv nextCont ->
      valueOfExpr handlerExpr (Env.extend x value savedEnv) nextCont

    NotSaved ->
      Left $ UncaughtException value


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
