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
-- The success and failure continuations are now represented using procedures rather than data.
--
-- The success continuation has two observers: applySuccessCont and applyHandler. Hence, in the procedural representation
-- we need to indicate how each continuation would behave under each observer. That's why the success continuations are
-- represented using scApply and scApplyHandler.
--
-- The failure continuation just has one observer: applyFailureCont.
--
-- Steps:
--
-- [x] Start with EXCEPTIONS
-- [x] Extend the data structure representation version to use 2 continuations
-- [x] Use a procedural representation
-- [ ] Inline the procedural representation
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
  valueOfExpr expr initEnv sEndCont fEndCont
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
      valueOfExpr aExpr env (diff1Cont bExpr env sCont) fCont

    Zero aExpr ->
      valueOfExpr aExpr env (zeroCont sCont) fCont

    If condition consequent alternative ->
      valueOfExpr condition env (ifCont consequent alternative env sCont) fCont

    Let x aExpr body ->
      valueOfExpr aExpr env (letCont x body env sCont) fCont

    Proc param body ->
      applySuccessCont sCont fCont (VProc $ Procedure param body env)

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) sCont fCont

    Call rator rand ->
      valueOfExpr rator env (ratorCont rand env sCont) fCont

    Try aExpr x handlerExpr ->
      valueOfExpr aExpr env (tryCont x handlerExpr env sCont) fCont

    Raise aExpr ->
      valueOfExpr aExpr env (raiseCont sCont) fCont


data SuccessCont
  = SuccessCont
      { scApply :: FailureCont -> Value -> Either RuntimeError Value
      , scApplyHandler :: FailureCont -> Value -> Either RuntimeError Value
      }


sEndCont :: SuccessCont
sEndCont =
  SuccessCont
    { scApply = \_ value ->
        trace "End of a successful computation" $
          Right value
    , scApplyHandler = \fCont value ->
        applyFailureCont fCont (UncaughtException value)
    }


zeroCont :: SuccessCont -> SuccessCont
zeroCont nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        zero value nextSCont fCont
    , scApplyHandler = \fCont value ->
        applyHandler nextSCont fCont value
    }


letCont :: Id -> Expr -> Env -> SuccessCont -> SuccessCont
letCont x body env nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        valueOfExpr body (Env.extend x value env) nextSCont fCont
    , scApplyHandler = \fCont value ->
        applyHandler nextSCont fCont value
    }


ifCont :: Expr -> Expr -> Env -> SuccessCont -> SuccessCont
ifCont consequent alternative env nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        computeIf value consequent alternative env nextSCont fCont
    , scApplyHandler = \fCont value ->
        applyHandler nextSCont fCont value
    }


diff1Cont :: Expr -> Env -> SuccessCont -> SuccessCont
diff1Cont bExpr env nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        valueOfExpr bExpr env (diff2Cont value nextSCont) fCont
    , scApplyHandler = \fCont value ->
        applyHandler nextSCont fCont value
    }


diff2Cont :: Value -> SuccessCont -> SuccessCont
diff2Cont aValue nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        diff aValue value nextSCont fCont
    , scApplyHandler = \fCont value ->
        applyHandler nextSCont fCont value
    }


ratorCont :: Expr -> Env -> SuccessCont -> SuccessCont
ratorCont rand env nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        valueOfExpr rand env (randCont value nextSCont) fCont
    , scApplyHandler = \fCont value ->
        applyHandler nextSCont fCont value
    }


randCont :: Value -> SuccessCont -> SuccessCont
randCont ratorValue nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        apply ratorValue value nextSCont fCont
    , scApplyHandler = \fCont value ->
        applyHandler nextSCont fCont value
    }


tryCont :: Id -> Expr -> Env -> SuccessCont -> SuccessCont
tryCont x handlerExpr savedEnv nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        applySuccessCont nextSCont fCont value
    , scApplyHandler = \fCont value ->
        valueOfExpr handlerExpr (Env.extend x value savedEnv) nextSCont fCont
    }


raiseCont :: SuccessCont -> SuccessCont
raiseCont nextSCont =
  SuccessCont
    { scApply = \fCont value ->
        applyHandler nextSCont fCont value
    , scApplyHandler = \fCont value ->
        applyHandler nextSCont fCont value
    }


applySuccessCont :: SuccessCont -> FailureCont -> Value -> Either RuntimeError Value
applySuccessCont sCont fCont value = scApply sCont fCont value


applyHandler :: SuccessCont -> FailureCont -> Value -> Either RuntimeError Value
applyHandler sCont fCont value = scApplyHandler sCont fCont value


data FailureCont
  = FailureCont (RuntimeError -> Either RuntimeError Value)


fEndCont :: FailureCont
fEndCont =
  FailureCont $ \err ->
    trace "End of a failed computation" $
        Left err


applyFailureCont :: FailureCont -> RuntimeError -> Either RuntimeError Value
applyFailureCont (FailureCont f) err = f err


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
