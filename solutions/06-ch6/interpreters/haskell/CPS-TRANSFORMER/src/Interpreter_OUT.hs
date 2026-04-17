module Interpreter_OUT (Value, run) where

import qualified Env

import AST.CPS_OUT
import Debug.Trace (trace)

data Value
  = NumberVal Number
  | BoolVal Bool
  | ProcedureVal Procedure

type Environment = Env.Env Id Value TfExpr

instance Show Value where
  show (NumberVal n) = show n
  show (BoolVal b) = if b then "True" else "False"
  show (ProcedureVal _) = "<<proc>>"

run :: Program -> Value
run = valueOfProgram

valueOfProgram :: Program -> Value
valueOfProgram (Program tfExpr) =
  valueOfTfExpr tfExpr initEnv
  where
    initEnv =
      Env.extend "i" (NumberVal 1)
        (Env.extend "v" (NumberVal 5)
          (Env.extend "x" (NumberVal 10)
            Env.empty))

valueOfTfExpr :: TfExpr -> Environment -> Value
valueOfTfExpr tfExpr env =
  case tfExpr of
    Simple simpleExpr ->
      valueOfSimpleExpr simpleExpr env

    If test consequent alternative ->
      let
        testVal = valueOfSimpleExpr test env
      in
        if (toBool testVal) then
          valueOfTfExpr consequent env
        else
          valueOfTfExpr alternative env

    Let var e body ->
      let
        val = valueOfSimpleExpr e env
      in
        valueOfTfExpr body (Env.extend var val env)

    Letrec recProcs e ->
      valueOfTfExpr e (Env.extendRec recProcs env)

    Call f args ->
      let
        fVal = valueOfSimpleExpr f env
        argVals = map (\arg -> valueOfSimpleExpr arg env) args
      in
        applyProcedure (toProcedure fVal) argVals

valueOfSimpleExpr :: SimpleExpr -> Environment -> Value
valueOfSimpleExpr simpleExpr env =
  case simpleExpr of
    Const n ->
      NumberVal n

    Var v ->
      Env.apply env v procedureVal

    Diff a b ->
      let
        aVal = valueOfSimpleExpr a env
        bVal = valueOfSimpleExpr b env
      in
        NumberVal (toNumber aVal - toNumber bVal)

    Zero e ->
      let
        val = valueOfSimpleExpr e env
      in
        BoolVal (toNumber val == 0)

    Proc var body ->
      procedureVal [var] body env

toNumber :: Value -> Number
toNumber (NumberVal n) = n
toNumber x = error ("Expected a number: " ++ show x)

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool x = error ("Expected a boolean: " ++ show x)

toProcedure :: Value -> Procedure
toProcedure (ProcedureVal p) = p
toProcedure x = error ("Expected a procedure: " ++ show x)

-- Procedure ADT

data Procedure = Procedure [Id] TfExpr Environment

procedure :: [Id] -> TfExpr -> Environment -> Procedure
procedure = Procedure

procedureVal :: [Id] -> TfExpr -> Environment -> Value
procedureVal vars body env =
  ProcedureVal (procedure vars body env)

applyProcedure :: Procedure -> [Value] -> Value
applyProcedure (Procedure vars body env) vals =
  let
    extend [] [] env = env
    extend _ [] env = error "Too few arguments"
    extend [] _ env = error "Too many arguments"
    extend (var:vars) (val:vals) env =
      extend vars vals (Env.extend var val env)
  in
    valueOfTfExpr body (extend vars vals env)
