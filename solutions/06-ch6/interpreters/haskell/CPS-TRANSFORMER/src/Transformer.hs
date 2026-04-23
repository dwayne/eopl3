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

        CPS_IN_AST.Var v ->
            --
            -- (k v)
            --
            CPS_OUT_AST.Call k [CPS_OUT_AST.Var v]

        CPS_IN_AST.Proc vars body ->
            let
                --
                -- kid is a fresh variable
                --
                -- I prefix an underscore so that it doesn't clash
                -- with any variable name the user might have used.
                --
                -- Why it works? Because the user isn't allowed to
                -- start their variable names with an underscore.
                --
                kid = "_k"
                kvar = CPS_OUT_AST.Var kid
            in
            CPS_OUT_AST.Call
                k
                [CPS_OUT_AST.Proc (vars ++ [ kid ]) (cpsOfExpr body kvar)]

        _ ->
            error "To be implemented"
