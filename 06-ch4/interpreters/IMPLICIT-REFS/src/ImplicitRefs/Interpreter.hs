module ImplicitRefs.Interpreter (Value, run) where

import qualified ImplicitRefs.Env as Env
import qualified ImplicitRefs.Store as Store

import ImplicitRefs.AST
import ImplicitRefs.Parser (parse)

data Value
  = NumberVal Number
  | BoolVal Bool
  | ProcedureVal Procedure

type Environment = Env.Env Id Store.Ref
type Store = Store.Store Value

instance Show Value where
  show (NumberVal n) = show n
  show (BoolVal b) = if b then "True" else "False"
  show (ProcedureVal _) = "<<proc>>"

run :: String -> Value
run = valueOfProgram . parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  fst (valueOfExpr expr initEnv initStore)
  where
    initEnv =
      Env.extend "i" iRef
        (Env.extend "v" vRef
          (Env.extend "x" xRef
            Env.empty))

    initStore = iStore

    (xRef, xStore) = Store.newref (NumberVal 10) Store.empty
    (vRef, vStore) = Store.newref (NumberVal 5) xStore
    (iRef, iStore) = Store.newref (NumberVal 1) vStore

valueOfExpr :: Expr -> Environment -> Store -> (Value, Store)
valueOfExpr expr env store =
  case expr of
    Const n ->
      (NumberVal n, store)

    Var v ->
      (Store.deref (Env.apply env v) store, store)

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
        (ref, store2) = Store.newref val store1
      in
        valueOfExpr body (Env.extend var ref env) store2

    Proc var body ->
      (procedureVal var body env, store)

    Call f arg ->
      let
        (fVal, store1) = valueOfExpr f env store
        (argVal, store2) = valueOfExpr arg env store1
      in
        applyProcedure (toProcedure fVal) argVal store2

    Letrec recProcs e ->
      let
        (nextEnv, nextStore) = extendRec recProcs env store

        extendRec [] env store = (env, store)
        extendRec ((name, param, body):recProcs) env store =
          let
            (ref, store1) =
              Store.newref (procedureVal param body nextEnv) store
          in
            extendRec recProcs (Env.extend name ref env) store1
      in
        valueOfExpr e nextEnv nextStore

    Begin exprs ->
      valueOfSequence exprs env store

    Assign var e ->
      let
        (val, store1) = valueOfExpr e env store
      in
        (val, Store.setref (Env.apply env var) val store1)

valueOfSequence :: [Expr] -> Environment -> Store -> (Value, Store)
valueOfSequence [e] env store = valueOfExpr e env store
valueOfSequence (e:es) env store =
  let
    (_, nextStore) = valueOfExpr e env store
  in
    valueOfSequence es env nextStore

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
procedure = Procedure

procedureVal :: Id -> Expr -> Environment -> Value
procedureVal var body env =
  ProcedureVal (procedure var body env)

applyProcedure :: Procedure -> Value -> Store -> (Value, Store)
applyProcedure (Procedure var body env) val store =
  let
    (ref, nextStore) = Store.newref val store
  in
    valueOfExpr body (Env.extend var ref env) nextStore
