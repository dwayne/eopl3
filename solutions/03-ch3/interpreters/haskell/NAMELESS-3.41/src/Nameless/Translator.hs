module Nameless.Translator (translate) where

import qualified Nameless.AST.AST as AST
import qualified Nameless.AST.Nameless as Nameless
import qualified Nameless.Env.Static as StaticEnv

type StaticEnv = StaticEnv.Env AST.Id

translate :: AST.Program -> Nameless.Program
translate (AST.Program expr) =
  Nameless.Program (translateExpr expr initSenv)
  where
    initSenv = StaticEnv.extend ["i", "v", "x"] StaticEnv.empty

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

    AST.Call f args ->
      Nameless.Call
        (translateExpr f senv)
        (map (flip translateExpr senv) args)

    AST.Var v ->
      Nameless.Var (StaticEnv.apply senv v)

    AST.Let bindings body ->
      Nameless.Let
        (map (flip translateExpr senv . snd) bindings)
        (translateExpr body (StaticEnv.extend (map fst bindings) senv))

    AST.Proc vars body ->
      Nameless.Proc
        (translateExpr body (StaticEnv.extend vars senv))
