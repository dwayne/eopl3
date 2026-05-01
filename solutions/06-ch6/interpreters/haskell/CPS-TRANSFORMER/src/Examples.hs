module Examples
    ( example0
    , example1
    , example2
    , example3
    , example4
    , example5
    , example6
    ) where

import AST.CPS_OUT (Program)
import Transformer (transform)


--
-- Examples 0 to 6 are taken from page 213.
--
-- In example2, example3, and example5 sum needs
-- to be replaced with + but it is not supported.
--
-- If + was supported then rather than
--
--    (sum arg1 arg2 ... argN _k)
--
-- we'd have
--
--    (_k (+ arg1 arg2 ... argN))
--


example0 :: Program
example0 = transform "proc (x) 17"
-- Program (Simple
--     (Proc ["x","_k"]
--         (Call (Var "_k") [Const 17])
--     )
-- )


example1 :: Program
example1 = transform "proc (x) (f -(x, 13) 7)"
-- Program (Simple
--     (Proc ["x","_k"]
--         (Call (Var "f") [Diff (Var "x") (Const 13),Const 7,Var "_k"])
--     )
-- )


example2 :: Program
example2 = transform "proc (x) (sum 22 -(x, 3) x)"
-- Program (Simple
--     (Proc ["x","_k"]
--         (Call (Var "sum") [Const 22,Diff (Var "x") (Const 3),Var "x",Var "_k"])
--     )
-- )


example3 :: Program
example3 = transform "proc (x) (sum 22 (f x) 37)"
-- Program (Simple
--     (Proc ["x","_k"]
--         (Call (Var "f")
--             [ Var "x"
--             , Proc ["_v0"]
--                 (Call (Var "sum") [Const 22,Var "_v0",Const 37,Var "_k"])
--             ]
--         )
--     )
-- )


example4 :: Program
example4 = transform "proc (x) (g 22 (f x))"
-- Program (Simple
--     (Proc ["x","_k"]
--         (Call (Var "f")
--             [ Var "x"
--             , Proc ["_v0"]
--                 (Call (Var "g") [Const 22,Var "_v0",Var "_k"])
--             ]
--         )
--     )
-- )


example5 :: Program
example5 = transform "proc (x) (sum 22 (f x) 33 (g y))"
-- Program (Simple
--     (Proc ["x","_k"]
--         (Call (Var "f")
--             [ Var "x"
--             , Proc ["_v0"]
--                 (Call (Var "g")
--                     [ Var "y"
--                     , Proc ["_v1"]
--                         (Call (Var "sum") [Const 22,Var "_v0",Const 33,Var "_v1",Var "_k"])
--                     ]
--                 )
--             ]
--         )
--     )
-- )


example6 :: Program
example6 = transform "proc (x) (h (f x) -(44, y) (g y))"
-- Program (Simple
--     (Proc ["x","_k"]
--         (Call (Var "f")
--             [ Var "x"
--             , Proc ["_v0"]
--                 (Call (Var "g")
--                     [ Var "y"
--                     , Proc ["_v1"]
--                         (Call (Var "h") [Var "_v0",Diff (Const 44) (Var "y"),Var "_v1",Var "_k"])
--                     ]
--                 )
--             ]
--         )
--     )
-- )
