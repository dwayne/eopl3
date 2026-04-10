{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
-- [x] Inline the procedural representation
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


type SuccessCont = FailureCont -> Value -> Either RuntimeError Value
type ExceptionCont = FailureCont -> Value -> Either RuntimeError Value
type FailureCont = RuntimeError -> Either RuntimeError Value


valueOfProgram :: Program -> Either RuntimeError Value
valueOfProgram (Program expr) =
  valueOfExpr
    expr
    initEnv
    (\_ value ->
      trace "End of a successful computation" $
        Right value
    )
    (\fCont value ->
      fCont (UncaughtException value)
    )
    (\err ->
      trace "End of a failed computation" $
        Left err
    )
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> SuccessCont -> ExceptionCont -> FailureCont -> Either RuntimeError Value
valueOfExpr expr env sCont eCont fCont =
  --
  -- sCont represents the success continuation under the applySuccessCont observer
  -- eCont represents the exception continuation under the applyHandler observer
  -- fCont represents the failure continuation under the applyFailureCont observer
  --
  case expr of
    Const n ->
      sCont fCont (VNumber n)

    Var x ->
      case Env.find x env of
        Just (Env.Value value) ->
          sCont fCont value

        Just (Env.Procedure param body savedEnv) ->
          sCont fCont (VProc $ Procedure param body savedEnv)

        Nothing ->
          fCont (IdentifierNotFound x)

    Diff aExpr bExpr ->
      valueOfExpr
        aExpr
        env
        (\fCont aValue ->
          valueOfExpr
            bExpr
            env
            (\fCont value ->
              diff aValue value sCont fCont
            )
            eCont
            fCont
        )
        eCont
        fCont

    Zero aExpr ->
      valueOfExpr
        aExpr
        env
        (\fCont value ->
          zero value sCont fCont
        )
        eCont
        fCont

    If condition consequent alternative ->
      valueOfExpr
        condition
        env
        (\fCont value ->
          computeIf value consequent alternative env sCont eCont fCont
        )
        eCont
        fCont

    Let x aExpr body ->
      valueOfExpr
        aExpr
        env
        (\fCont value ->
          valueOfExpr body (Env.extend x value env) sCont eCont fCont
        )
        eCont
        fCont

    Proc param body ->
      sCont fCont (VProc $ Procedure param body env)

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) sCont eCont fCont

    Call rator rand ->
      valueOfExpr
        rator
        env
        (\fCont ratorValue ->
          valueOfExpr
            rand
            env
            (\fCont value ->
              apply ratorValue value sCont eCont fCont
            )
            eCont
            fCont
        )
        eCont
        fCont

    Try aExpr x handlerExpr ->
      valueOfExpr
        aExpr
        env
        sCont
        (\fCont value ->
          valueOfExpr handlerExpr (Env.extend x value env) sCont eCont fCont
        )
        fCont

    Raise aExpr ->
      valueOfExpr
        aExpr
        env
        eCont -- N.B. Not sCont but eCont
        eCont
        fCont


zero :: Value -> SuccessCont -> FailureCont -> Either RuntimeError Value
zero aValue sCont fCont =
  case toNumber aValue of
    Right a ->
      sCont fCont (VBool $ a == 0)

    Left err ->
      fCont err


computeIf :: Value -> Expr -> Expr -> Env -> SuccessCont -> ExceptionCont -> FailureCont -> Either RuntimeError Value
computeIf conditionValue consequent alternative env sCont eCont fCont =
  case toBool conditionValue of
    Right b ->
      let
        expr = if b then consequent else alternative
      in
      valueOfExpr expr env sCont eCont fCont

    Left err ->
      fCont err


diff :: Value -> Value -> SuccessCont -> FailureCont -> Either RuntimeError Value
diff aValue bValue sCont fCont =
  case toNumber aValue of
    Right a ->
      case toNumber bValue of
          Right b ->
            sCont fCont (VNumber $ a - b)

          Left err ->
            fCont err

    Left err ->
      fCont err


apply :: Value -> Value -> SuccessCont -> ExceptionCont -> FailureCont -> Either RuntimeError Value
apply ratorValue arg sCont eCont fCont =
  case toProcedure ratorValue of
    Right (Procedure param body savedEnv) ->
      valueOfExpr body (Env.extend param arg savedEnv) sCont eCont fCont

    Left err ->
      fCont err


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
