module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


--
-- Exercise 5.36
--
-- An alternative design that also avoids the linear search in apply-handler
-- is to use two continuations, a normal continuation and an exception
-- continuation.
--
-- This implementation modifies the interpreter to take two continuations
-- instead of one.
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
  valueOfExpr expr initEnv EndCont EndXCont
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> Cont -> XCont -> Either RuntimeError Value
valueOfExpr expr env cont xcont =
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
      valueOfExpr aExpr env (Diff1Cont bExpr env cont xcont) xcont

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont cont) xcont

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env cont xcont) xcont

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env cont xcont) xcont

    Proc param body ->
      applyCont cont $ Right $ VProc $ Procedure param body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) cont xcont

    Call rator rand ->
      valueOfExpr rator env (RatorCont rand env cont xcont) xcont

    Try aExpr x handlerExpr ->
      valueOfExpr aExpr env (TryCont cont) (TryXCont x handlerExpr env cont xcont)

    Raise aExpr ->
      valueOfExpr aExpr env (RaiseCont xcont) xcont


data Cont
  = EndCont
  | ZeroCont Cont
  | LetCont Id Expr Env Cont XCont
  | IfCont Expr Expr Env Cont XCont
  | Diff1Cont Expr Env Cont XCont
  | Diff2Cont Value Cont
  | RatorCont Expr Env Cont XCont
  | RandCont Value Cont XCont
  | TryCont Cont
  | RaiseCont XCont


data XCont
  = EndXCont
  | TryXCont Id Expr Env Cont XCont


applyCont :: Cont -> Either RuntimeError Value -> Either RuntimeError Value
applyCont cont input = do
  value <- input
  case cont of
    EndCont ->
      trace "End of computation" $
        Right value

    ZeroCont nextCont ->
      applyCont nextCont $ zero value

    LetCont x body env nextCont xcont ->
      valueOfExpr body (Env.extend x value env) nextCont xcont

    IfCont consequent alternative env nextCont xcont ->
      computeIf value consequent alternative env nextCont xcont

    Diff1Cont bExpr env nextCont xcont ->
      valueOfExpr bExpr env (Diff2Cont value nextCont) xcont

    Diff2Cont aValue nextCont ->
      applyCont nextCont $ diff aValue value

    RatorCont rand env nextCont xcont ->
      valueOfExpr rand env (RandCont value nextCont xcont) xcont

    RandCont ratorValue nextCont xcont ->
      apply ratorValue value nextCont xcont

    TryCont nextCont ->
      applyCont nextCont input

    RaiseCont xcont ->
      applyHandler xcont value


applyHandler :: XCont -> Value -> Either RuntimeError Value
applyHandler xcont value =
  case xcont of
    TryXCont x handlerExpr savedEnv nextCont nextXCont ->
      valueOfExpr handlerExpr (Env.extend x value savedEnv) nextCont nextXCont

    EndXCont ->
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


computeIf :: Value -> Expr -> Expr -> Env -> Cont -> XCont -> Either RuntimeError Value
computeIf conditionValue consequent alternative env cont xcont = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env cont xcont


apply :: Value -> Value -> Cont -> XCont -> Either RuntimeError Value
apply ratorValue arg cont xcont = do
  Procedure param body savedEnv <- toProcedure ratorValue
  valueOfExpr body (Env.extend param arg savedEnv) cont xcont


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
