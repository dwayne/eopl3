module Transformer (cpsOfExpr) where

import qualified AST.CPS_IN as CPS_IN_AST
import qualified AST.CPS_OUT as CPS_OUT_AST


cpsOfExpr :: CPS_IN_AST.Expr -> CPS_OUT_AST.SimpleExpr -> CPS_OUT_AST.TfExpr
cpsOfExpr expr k =
    case expr of
        CPS_IN_AST.Const n ->
            --
            -- (k n)
            --
            CPS_OUT_AST.Call k [CPS_OUT_AST.Const n]

        _ ->
            error "To be implemented"
