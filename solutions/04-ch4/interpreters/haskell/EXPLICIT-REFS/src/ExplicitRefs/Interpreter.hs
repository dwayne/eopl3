module ExplicitRefs.Interpreter (Value, run) where

import qualified ExplicitRefs.Env as Env
import qualified ExplicitRefs.Store as Store

import ExplicitRefs.AST
import ExplicitRefs.Parser (parse)

data Value
  = NumberVal Number
  | BoolVal Bool
  | ProcedureVal Procedure
  | RefVal Store.Ref

type Environment = Env.Env Id Value Expr
type Store = Store.Store Value

instance Show Value where
  show (NumberVal n) = show n
  show (BoolVal b) = if b then "True" else "False"
  show (ProcedureVal _) = "<<proc>>"
  show (RefVal r) = "<<ref:" ++ show r ++ ">>"

run :: String -> Value
run = valueOfProgram . parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  fst (valueOfExpr expr initEnv initStore)
  where
    initEnv =
      Env.extend "i" (NumberVal 1)
        (Env.extend "v" (NumberVal 5)
          (Env.extend "x" (NumberVal 10)
            Env.empty))

    initStore = Store.empty

valueOfExpr :: Expr -> Environment -> Store -> (Value, Store)
valueOfExpr expr env store =
  case expr of
    Const n ->
      (NumberVal n, store)

    Var v ->
      (Env.apply env v procedureVal, store)

    Diff a b ->
      let
        (aVal, store1) = valueOfExpr a env store
        (bVal, store2) = valueOfExpr b env store1
      in
        (NumberVal (toNumber aVal - toNumber bVal), store2)

    Zero e ->
      let
        (val, store1) = valueOfExpr e env store
      in
        (BoolVal (toNumber val == 0), store1)

    If test consequent alternative ->
      let
        (testVal, store1) = valueOfExpr test env store
      in
        if (toBool testVal) then
          valueOfExpr consequent env store1
        else
          valueOfExpr alternative env store1

    Let var e body ->
      let
        (val, store1) = valueOfExpr e env store
      in
        valueOfExpr body (Env.extend var val env) store1

    Proc var body ->
      (procedureVal var body env, store)

    Call f arg ->
      let
        (fVal, store1) = valueOfExpr f env store
        (argVal, store2) = valueOfExpr arg env store1
      in
        applyProcedure (toProcedure fVal) argVal store2

    Letrec recProcs e ->
      valueOfExpr e (Env.extendRec recProcs env) store

    Newref e ->
      let
        (val, store1) = valueOfExpr e env store
        (ref, store2) = Store.newref val store1
      in
        (RefVal ref, store2)

    Deref ref ->
      let
        (refVal, store1) = valueOfExpr ref env store
        val = Store.deref (toRef refVal) store1
      in
        (val, store1)

    Setref ref e ->
      let
        (refVal, store1) = valueOfExpr ref env store
        (val, store2) = valueOfExpr e env store1
        store3 = Store.setref (toRef refVal) val store2
      in
        (val, store3)

toNumber :: Value -> Number
toNumber (NumberVal n) = n
toNumber x = error ("Expected a number: " ++ show x)

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool x = error ("Expected a boolean: " ++ show x)

toProcedure :: Value -> Procedure
toProcedure (ProcedureVal p) = p
toProcedure x = error ("Expected a procedure: " ++ show x)

toRef :: Value -> Store.Ref
toRef (RefVal r) = r
toRef x = error ("Expected a reference: " ++ show x)

-- Procedure ADT

data Procedure = Procedure Id Expr Environment

procedure :: Id -> Expr -> Environment -> Procedure
procedure = Procedure

procedureVal :: Id -> Expr -> Environment -> Value
procedureVal var body env =
  ProcedureVal (procedure var body env)

applyProcedure :: Procedure -> Value -> Store -> (Value, Store)
applyProcedure (Procedure var body env) val store =
  valueOfExpr body (Env.extend var val env) store
