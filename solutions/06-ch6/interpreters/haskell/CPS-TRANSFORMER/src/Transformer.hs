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


cpsOfExprs :: [CPS_IN_AST.Expr] -> ([CPS_OUT_AST.SimpleExpr] -> CPS_OUT_AST.TfExpr) -> CPS_OUT_AST.TfExpr
cpsOfExprs exprs build =
    case findFirstNonSimpleExpr exprs of
        Nothing ->
            --
            -- All are simple.
            --
            build (map toSimpleExpr exprs)

        Just (before, expr, after) ->
            --
            -- before is a list of simple expressions
            -- expr is a non-simple expression
            -- after is a list of simple/non-simple expressions
            --
            let
                --
                -- N.B. This isn't quite right. vid needs to be unique.
                --
                vid = "_v"
                vvar = CPS_IN_AST.Var vid
            in
            cpsOfExpr expr (CPS_OUT_AST.Proc [vid] (cpsOfExprs (before ++ [vvar] ++ after) build))



isSimpleExpr :: CPS_IN_AST.Expr -> Bool
isSimpleExpr expr =
    case expr of
        CPS_IN_AST.Const _ ->
            True

        CPS_IN_AST.Var _ ->
            True

        CPS_IN_AST.Diff a b ->
            isSimpleExpr a && isSimpleExpr b

        CPS_IN_AST.Zero e ->
            isSimpleExpr e

        CPS_IN_AST.Proc _ _ ->
            True

        _ ->
            False


toSimpleExpr :: CPS_IN_AST.Expr -> CPS_OUT_AST.SimpleExpr
toSimpleExpr expr =
    --
    -- N.B. Assumes isSimpleExpr expr is true.
    --
    case expr of
        CPS_IN_AST.Const n ->
            CPS_OUT_AST.Const n

        CPS_IN_AST.Var v ->
            CPS_OUT_AST.Var v

        CPS_IN_AST.Diff a b ->
            CPS_OUT_AST.Diff (toSimpleExpr a) (toSimpleExpr b)

        CPS_IN_AST.Zero e ->
            CPS_OUT_AST.Zero (toSimpleExpr e)

        CPS_IN_AST.Proc vars body ->
            let
                --
                -- See previous note about kid.
                --
                kid = "_k"
                kvar = CPS_OUT_AST.Var kid
            in
            CPS_OUT_AST.Proc (vars ++ [kid]) (cpsOfExpr body kvar)

        _ ->
            --
            -- If the algorithm is correct then this should never happen.
            --
            error "Logical error"


findFirstNonSimpleExpr :: [CPS_IN_AST.Expr] -> Maybe ([CPS_IN_AST.Expr], CPS_IN_AST.Expr, [CPS_IN_AST.Expr])
findFirstNonSimpleExpr =
    findFirstNonSimpleExprHelper []


findFirstNonSimpleExprHelper :: [CPS_IN_AST.Expr] -> [CPS_IN_AST.Expr] -> Maybe ([CPS_IN_AST.Expr], CPS_IN_AST.Expr, [CPS_IN_AST.Expr])
findFirstNonSimpleExprHelper verp exprs =
    case exprs of
        [] ->
            Nothing

        expr : restOfExprs ->
            if isSimpleExpr expr then
                findFirstNonSimpleExprHelper (expr : verp) restOfExprs

            else
                Just
                    ( reverse verp
                    , expr
                    , restOfExprs
                    )
