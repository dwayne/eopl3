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
  valueOfExpr expr initEnv EndCont NoHandler
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> Cont -> ExceptionHandler -> Either RuntimeError Value
valueOfExpr expr env cont handler =
  case expr of
    Const n ->
      applyCont cont handler $ Right $ VNumber n

    Var x ->
      applyCont cont handler $
        case Env.find x env of
          Just (Env.Value value) ->
            Right value

          Just (Env.Procedure param body savedEnv) ->
            Right $ VProc $ Procedure param body savedEnv

          Nothing ->
            Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (Diff1Cont bExpr env cont) handler

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont cont) handler

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env cont) handler

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env cont) handler

    Proc param body ->
      applyCont cont handler $ Right $ VProc $ Procedure param body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) cont handler

    Call rator rand ->
      valueOfExpr rator env (RatorCont rand env cont) handler

    Try aExpr x handlerExpr ->
      valueOfExpr aExpr env (TryCont cont) (Handler x handlerExpr env cont handler)

    Raise aExpr ->
      valueOfExpr aExpr env (RaiseCont handler) handler


data Cont
  = EndCont
  | ZeroCont Cont
  | LetCont Id Expr Env Cont
  | IfCont Expr Expr Env Cont
  | Diff1Cont Expr Env Cont
  | Diff2Cont Value Cont
  | RatorCont Expr Env Cont
  | RandCont Value Cont
  | TryCont Cont
  | RaiseCont ExceptionHandler


data ExceptionHandler
  = NoHandler
  | Handler Id Expr Env Cont ExceptionHandler


applyCont :: Cont -> ExceptionHandler -> Either RuntimeError Value -> Either RuntimeError Value
applyCont cont handler input = do
  value <- input
  case cont of
    EndCont ->
      trace "End of computation" $
        Right value

    ZeroCont nextCont ->
      applyCont nextCont handler $ zero value

    LetCont x body env nextCont ->
      valueOfExpr body (Env.extend x value env) nextCont handler

    IfCont consequent alternative env nextCont ->
      computeIf value consequent alternative env nextCont handler

    Diff1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Diff2Cont value nextCont) handler

    Diff2Cont aValue nextCont ->
      applyCont nextCont handler $ diff aValue value

    RatorCont rand env nextCont ->
      valueOfExpr rand env (RandCont value nextCont) handler

    RandCont ratorValue nextCont ->
      apply ratorValue value nextCont handler

    TryCont nextCont ->
      applyCont nextCont handler input

    RaiseCont savedHandler ->
      -- N.B. We need to use savedHandler here.
      --
      -- Consider the value of:
      --
      -- try raise try 1 catch (x) 2 catch (y) y
      --
      -- With savedHandler it's 1.
      -- With handler it's 2.
      --
      -- Clearly it should be 1.
      applyHandler savedHandler value


applyHandler :: ExceptionHandler -> Value -> Either RuntimeError Value
applyHandler handler value =
  case handler of
    Handler x handlerExpr savedEnv nextCont nextHandler ->
      valueOfExpr handlerExpr (Env.extend x value savedEnv) nextCont nextHandler

    NoHandler ->
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


computeIf :: Value -> Expr -> Expr -> Env -> Cont -> ExceptionHandler -> Either RuntimeError Value
computeIf conditionValue consequent alternative env cont handler = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env cont handler


apply :: Value -> Value -> Cont -> ExceptionHandler -> Either RuntimeError Value
apply ratorValue arg cont handler = do
  Procedure param body savedEnv <- toProcedure ratorValue
  valueOfExpr body (Env.extend param arg savedEnv) cont handler


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
