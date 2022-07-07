module Nameless.Translator (translate) where

import qualified Nameless.AST.AST as AST
import qualified Nameless.AST.Nameless as Nameless
import qualified Nameless.Env.Static as StaticEnv

type StaticEnv = StaticEnv.Env AST.Id

translate :: AST.Program -> Nameless.Program
translate (AST.Program expr) =
  Nameless.Program (translateExpr expr initSenv)
  where
    initSenv =
      StaticEnv.extend "i"
        (StaticEnv.extend "v"
          (StaticEnv.extend "x"
            StaticEnv.empty))

translateExpr :: AST.Expr -> StaticEnv -> Nameless.Expr
translateExpr expr senv =
  case expr of
    AST.Const n ->
      Nameless.Const n

    AST.Diff a b ->
      Nameless.Diff (translateExpr a senv) (translateExpr b senv)

    AST.Zero e ->
      Nameless.Zero (translateExpr e senv)

    AST.If test consequent alternative ->
      Nameless.If
        (translateExpr test senv)
        (translateExpr consequent senv)
        (translateExpr alternative senv)

    AST.Cond clauses ->
      let
        translateClause (test, consequent) =
          ( translateExpr test senv
          , translateExpr consequent senv
          )
      in
        Nameless.Cond (map translateClause clauses)

    AST.Call f arg ->
      Nameless.Call
        (translateExpr f senv)
        (translateExpr arg senv)

    AST.Var v ->
      Nameless.Var (StaticEnv.apply senv v)

    AST.Let var e body ->
      Nameless.Let
        (translateExpr e senv)
        (translateExpr body (StaticEnv.extend var senv))

    AST.Proc var body ->
      Nameless.Proc
        (translateExpr body (StaticEnv.extend var senv))
