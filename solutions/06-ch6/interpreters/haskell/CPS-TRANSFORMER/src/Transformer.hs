module Transformer (cpsOfExpr) where

import qualified AST.CPS_IN as CPS_IN_AST
import qualified AST.CPS_OUT as CPS_OUT_AST


cpsOfExpr :: CPS_IN_AST.Expr -> CPS_OUT_AST.SimpleExpr -> Int -> ( CPS_OUT_AST.TfExpr, Int )
cpsOfExpr expr k counter0 =
    case expr of
        CPS_IN_AST.Const n ->
            --
            -- (k n)
            --
            ( CPS_OUT_AST.Call k [CPS_OUT_AST.Const n]
            , counter0
            )

        CPS_IN_AST.Var v ->
            --
            -- (k v)
            --
            ( CPS_OUT_AST.Call k [CPS_OUT_AST.Var v]
            , counter0
            )

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

                ( tfbody, counter1 ) = cpsOfExpr body kvar counter0
            in
            ( CPS_OUT_AST.Call
                k
                [CPS_OUT_AST.Proc (vars ++ [ kid ]) tfbody]
            , counter1
            )

        CPS_IN_AST.Call f args ->
            cpsOfExprs
                (f : args)
                (\(simpleF : simpleArgs) ->
                    CPS_OUT_AST.Call simpleF (simpleArgs ++ [k])
                )
                counter0

        CPS_IN_AST.Zero e ->
            cpsOfExprs
                [e]
                (\[simpleE] ->
                    CPS_OUT_AST.Simple (CPS_OUT_AST.Zero simpleE)
                )
                counter0

        CPS_IN_AST.Diff a b ->
            cpsOfExprs
                [a, b]
                (\[simpleA, simpleB] ->
                    CPS_OUT_AST.Simple (CPS_OUT_AST.Diff simpleA simpleB)
                )
                counter0

        CPS_IN_AST.If test consequence alternative ->
            --
            -- N.B. I didn't like how I had to compute the cpsOfExpr for
            -- consequence and alternative before the test expression.
            -- I wanted to do it within the callback but then the counters
            -- wouldn't propagate correctly.
            --
            let
                ( consequenceTf, counter1 ) = cpsOfExpr consequence k counter0
                ( alternativeTf, counter2 ) = cpsOfExpr alternative k counter1
            in
            cpsOfExprs
                [test]
                (\[simpleTest] ->
                    CPS_OUT_AST.If simpleTest consequenceTf alternativeTf
                )
                counter2

        _ ->
            error "To be implemented"


cpsOfExprs :: [CPS_IN_AST.Expr] -> ([CPS_OUT_AST.SimpleExpr] -> CPS_OUT_AST.TfExpr) -> Int -> ( CPS_OUT_AST.TfExpr, Int )
cpsOfExprs exprs build counter0 =
    case findFirstNonSimpleExpr exprs of
        Nothing ->
            --
            -- All are simple.
            --
            let
                ( simpleExprs, counter1 ) = toSimpleExprs exprs counter0
            in
            ( build simpleExprs
            , counter1
            )

        Just (before, expr, after) ->
            --
            -- before is a list of simple expressions
            -- expr is a non-simple expression
            -- after is a list of simple/non-simple expressions
            --
            let
                vid = "_v" ++ show counter0
                vvar = CPS_IN_AST.Var vid

                ( body, counter1 ) = cpsOfExprs (before ++ [vvar] ++ after) build counter0
            in
            cpsOfExpr expr (CPS_OUT_AST.Proc [vid] body) counter1


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


toSimpleExprs :: [CPS_IN_AST.Expr] -> Int -> ( [CPS_OUT_AST.SimpleExpr], Int )
toSimpleExprs exprs counter0 =
    toSimpleExprsHelper [] exprs counter0


toSimpleExprsHelper :: [CPS_OUT_AST.SimpleExpr] -> [CPS_IN_AST.Expr] -> Int -> ( [CPS_OUT_AST.SimpleExpr], Int )
toSimpleExprsHelper simpleExprs exprs counter0 =
    case exprs of
        [] ->
            ( reverse simpleExprs, counter0 )

        expr : restExprs ->
            let
                ( simpleExpr, counter1 ) = toSimpleExpr expr counter0
            in
            toSimpleExprsHelper (simpleExpr : simpleExprs) restExprs counter1


toSimpleExpr :: CPS_IN_AST.Expr -> Int -> ( CPS_OUT_AST.SimpleExpr, Int )
toSimpleExpr expr counter0 =
    --
    -- N.B. Assumes isSimpleExpr expr is true.
    --
    case expr of
        CPS_IN_AST.Const n ->
            ( CPS_OUT_AST.Const n
            , counter0
            )

        CPS_IN_AST.Var v ->
            ( CPS_OUT_AST.Var v
            , counter0
            )

        CPS_IN_AST.Diff a b ->
            let
                ( simpleA, counter1 ) = toSimpleExpr a counter0
                ( simpleB, counter2 ) = toSimpleExpr b counter1
            in
            ( CPS_OUT_AST.Diff simpleA simpleB
            , counter2
            )

        CPS_IN_AST.Zero e ->
            let
                ( simpleE, counter1 ) = toSimpleExpr e counter0
            in
            ( CPS_OUT_AST.Zero simpleE
            , counter1
            )

        CPS_IN_AST.Proc vars body ->
            let
                --
                -- See previous note about kid.
                --
                kid = "_k"
                kvar = CPS_OUT_AST.Var kid

                ( tfbody, counter1 ) = cpsOfExpr body kvar counter0
            in
            ( CPS_OUT_AST.Proc (vars ++ [kid]) tfbody
            , counter1
            )

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
