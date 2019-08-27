module Nameless.Interpreter (Value, run) where

import qualified Nameless.AST.AST as AST
import qualified Nameless.Env.Nameless as Env

import Nameless.AST.Nameless
import Nameless.Parser (parse)
import Nameless.Translator (translate)

data Value
  = NumberVal Number
  | BoolVal Bool
  | ProcedureVal Procedure

type Environment = Env.Env Lexaddr Value

instance Show Value where
  show (NumberVal n) = show n
  show (BoolVal b) = if b then "True" else "False"
  show (ProcedureVal _) = "<<proc>>"

run :: String -> Value
run = valueOfProgram . translate . parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend (NumberVal 1)
        (Env.extend (NumberVal 5)
          (Env.extend (NumberVal 10)
            Env.empty))

valueOfExpr :: Expr -> Environment -> Value
valueOfExpr expr env =
  case expr of
    Const n ->
      NumberVal n

    Var n ->
      Env.apply env n

    Diff a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        NumberVal (toNumber aVal - toNumber bVal)

    Zero e ->
      let
        val = valueOfExpr e env
      in
        BoolVal (toNumber val == 0)

    If test consequent alternative ->
      let
        testVal = valueOfExpr test env
      in
        if (toBool testVal) then
          valueOfExpr consequent env
        else
          valueOfExpr alternative env

    Let e body ->
      let
        val = valueOfExpr e env
      in
        valueOfExpr body (Env.extend val env)

    Proc body ->
      ProcedureVal (procedure body env)

    Call f arg ->
      let
        fVal = valueOfExpr f env
        argVal = valueOfExpr arg env
      in
        applyProcedure (toProcedure fVal) argVal

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

data Procedure = Procedure Expr Environment

procedure :: Expr -> Environment -> Procedure
procedure = Procedure

applyProcedure :: Procedure -> Value -> Value
applyProcedure (Procedure body env) val =
  valueOfExpr body (Env.extend val env)
