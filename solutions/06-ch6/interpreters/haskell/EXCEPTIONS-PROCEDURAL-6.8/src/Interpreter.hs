module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


--
-- I think this implementation is the correct implementation of the continuation-passing style version of EXCEPTIONS.
--
-- It requires two continuations, a success and a failure continuation, to ensure that every function call is in tail position.
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
  valueOfExpr expr initEnv SEndCont FEndCont
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> SuccessCont -> FailureCont -> Either RuntimeError Value
valueOfExpr expr env sCont fCont =
  case expr of
    Const n ->
      applySuccessCont sCont fCont (VNumber n)

    Var x ->
      case Env.find x env of
        Just (Env.Value value) ->
          applySuccessCont sCont fCont value

        Just (Env.Procedure param body savedEnv) ->
          applySuccessCont sCont fCont (VProc $ Procedure param body savedEnv)

        Nothing ->
          applyFailureCont fCont (IdentifierNotFound x)

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (Diff1Cont bExpr env sCont) fCont

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont sCont) fCont

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env sCont) fCont

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env sCont) fCont

    Proc param body ->
      applySuccessCont sCont fCont (VProc $ Procedure param body env)

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) sCont fCont

    Call rator rand ->
      valueOfExpr rator env (RatorCont rand env sCont) fCont

    Try aExpr x handlerExpr ->
      valueOfExpr aExpr env (TryCont x handlerExpr env sCont) fCont

    Raise aExpr ->
      valueOfExpr aExpr env (RaiseCont sCont) fCont


data SuccessCont
  = SEndCont
  | ZeroCont SuccessCont
  | LetCont Id Expr Env SuccessCont
  | IfCont Expr Expr Env SuccessCont
  | Diff1Cont Expr Env SuccessCont
  | Diff2Cont Value SuccessCont
  | RatorCont Expr Env SuccessCont
  | RandCont Value SuccessCont
  | TryCont Id Expr Env SuccessCont
  | RaiseCont SuccessCont



data FailureCont
  = FEndCont


applySuccessCont :: SuccessCont -> FailureCont -> Value -> Either RuntimeError Value
applySuccessCont sCont fCont value =
  case sCont of
    SEndCont ->
      trace "End of a successful computation" $
        Right value

    ZeroCont nextSCont ->
      zero value nextSCont fCont

    LetCont x body env nextSCont ->
      valueOfExpr body (Env.extend x value env) nextSCont fCont

    IfCont consequent alternative env nextSCont ->
      computeIf value consequent alternative env nextSCont fCont

    Diff1Cont bExpr env nextSCont ->
      valueOfExpr bExpr env (Diff2Cont value nextSCont) fCont

    Diff2Cont aValue nextSCont ->
      diff aValue value nextSCont fCont

    RatorCont rand env nextSCont ->
      valueOfExpr rand env (RandCont value nextSCont) fCont

    RandCont ratorValue nextSCont ->
      apply ratorValue value nextSCont fCont

    TryCont _ _ _ nextSCont ->
      applySuccessCont nextSCont fCont value

    RaiseCont nextSCont ->
      applyHandler nextSCont fCont value


applyHandler :: SuccessCont -> FailureCont -> Value -> Either RuntimeError Value
applyHandler sCont fCont value =
  case sCont of
    TryCont x handlerExpr savedEnv nextSCont ->
      valueOfExpr handlerExpr (Env.extend x value savedEnv) nextSCont fCont

    SEndCont ->
      applyFailureCont fCont (UncaughtException value)

    ZeroCont nextSCont ->
      applyHandler nextSCont fCont value

    LetCont _ _ _ nextSCont ->
      applyHandler nextSCont fCont value

    IfCont _ _ _ nextSCont ->
      applyHandler nextSCont fCont value

    Diff1Cont _ _ nextSCont ->
      applyHandler nextSCont fCont value

    Diff2Cont _ nextSCont ->
      applyHandler nextSCont fCont value

    RatorCont _ _ nextSCont ->
      applyHandler nextSCont fCont value

    RandCont _ nextSCont ->
      applyHandler nextSCont fCont value

    RaiseCont nextSCont ->
      applyHandler nextSCont fCont value


applyFailureCont :: FailureCont -> RuntimeError -> Either RuntimeError Value
applyFailureCont fCont err =
  case fCont of
    FEndCont ->
      trace "End of a failed computation" $
        Left err


zero :: Value -> SuccessCont -> FailureCont -> Either RuntimeError Value
zero aValue sCont fCont =
  case toNumber aValue of
    Right a ->
      applySuccessCont sCont fCont (VBool $ a == 0)

    Left err ->
      applyFailureCont fCont err


computeIf :: Value -> Expr -> Expr -> Env -> SuccessCont -> FailureCont -> Either RuntimeError Value
computeIf conditionValue consequent alternative env sCont fCont =
  case toBool conditionValue of
    Right b ->
      let
        expr = if b then consequent else alternative
      in
      valueOfExpr expr env sCont fCont

    Left err ->
      applyFailureCont fCont err


diff :: Value -> Value -> SuccessCont -> FailureCont -> Either RuntimeError Value
diff aValue bValue sCont fCont =
  case toNumber aValue of
    Right a ->
      case toNumber bValue of
          Right b ->
            applySuccessCont sCont fCont (VNumber $ a - b)

          Left err ->
            applyFailureCont fCont err

    Left err ->
      applyFailureCont fCont err


apply :: Value -> Value -> SuccessCont -> FailureCont -> Either RuntimeError Value
apply ratorValue arg sCont fCont =
  case toProcedure ratorValue of
    Right (Procedure param body savedEnv) ->
      valueOfExpr body (Env.extend param arg savedEnv) sCont fCont

    Left err ->
      applyFailureCont fCont err


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
