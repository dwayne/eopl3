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

    AST.Cons h t ->
      Nameless.Cons (translateExpr h senv) (translateExpr t senv)

    AST.Car l ->
      Nameless.Car (translateExpr l senv)

    AST.Cdr l ->
      Nameless.Cdr (translateExpr l senv)

    AST.Null l ->
      Nameless.Null (translateExpr l senv)

    AST.Empty ->
      Nameless.Empty

    AST.If test consequent alternative ->
      Nameless.If
        (translateExpr test senv)
        (translateExpr consequent senv)
        (translateExpr alternative senv)

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

    AST.Unpack vars e body ->
      let
        extendMany [] env = env
        extendMany (var:vars) env =
          extendMany vars (StaticEnv.extend var env)

        nVars = length vars
        nVals = consLength e
      in
        if nVals > nVars then
          error "unpack: list is too long"
        else if nVals < nVars then
          error "unpack: list is too short"
        else
          Nameless.Unpack
            (translateExpr e senv)
            (translateExpr body (extendMany vars senv))

    AST.Proc var body ->
      Nameless.Proc
        (translateExpr body (StaticEnv.extend var senv))

consLength :: AST.Expr -> Int
consLength AST.Empty = 0
consLength (AST.Cons _ t) = 1 + consLength t
consLength _ = error "Expected a list"
