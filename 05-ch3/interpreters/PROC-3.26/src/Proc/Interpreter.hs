module Proc.Interpreter (Value, run) where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Proc.Env as Env

import Proc.AST
import Proc.Parser (parse)

data Value
  = NumberVal Number
  | BoolVal Bool
  | ProcedureVal Procedure

type Environment = Env.Env Id Value

instance Show Value where
  show (NumberVal n) = show n
  show (BoolVal b) = if b then "True" else "False"
  show (ProcedureVal _) = "<<proc>>"

run :: String -> Value
run = valueOfProgram . parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend "i" (NumberVal 1)
        (Env.extend "v" (NumberVal 5)
          (Env.extend "x" (NumberVal 10)
            Env.empty))

valueOfExpr :: Expr -> Environment -> Value
valueOfExpr expr env =
  case expr of
    Const n ->
      NumberVal n

    Var v ->
      Env.apply env v

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

    Let var e body ->
      let
        val = valueOfExpr e env
      in
        valueOfExpr body (Env.extend var val env)

    Proc var body ->
      ProcedureVal (procedure var body env)

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

data Procedure = Procedure Id Expr Environment

procedure :: Id -> Expr -> Environment -> Procedure
procedure var body env =
  let
    savedEnv = pluck (freeVariables (Proc var body)) env
  in
    Procedure var body savedEnv

pluck :: Set Id -> Environment -> Environment
pluck vars global =
  let
    extend var env = Env.extend var (Env.apply global var) env
  in
    Set.foldr extend Env.empty vars

freeVariables :: Expr -> Set Id
freeVariables expr =
  case expr of
    Const n ->
      Set.empty

    Var v ->
      Set.singleton v

    Diff a b ->
      Set.union (freeVariables a) (freeVariables b)

    Zero e ->
      freeVariables e

    If test consequent alternative ->
      Set.unions
        [ freeVariables test
        , freeVariables consequent
        , freeVariables alternative
        ]

    Let var e body ->
      Set.union
        (freeVariables e)
        (Set.difference (freeVariables body) (Set.singleton var))

    Proc var body ->
      Set.difference (freeVariables body) (Set.singleton var)

    Call f arg ->
      Set.union (freeVariables f) (freeVariables arg)

applyProcedure :: Procedure -> Value -> Value
applyProcedure (Procedure var body env) val =
  valueOfExpr body (Env.extend var val env)
