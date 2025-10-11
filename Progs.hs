module Progs (prog, prog2) where

import qualified Term as T

prog :: T.Term
prog =
  T.Seq
    (T.Let "x" (T.Literal 10))
    ( T.While
        (T.Var "x")
        ( T.Seq
            (T.Write (T.Var "x"))
            (T.Let "x" (T.Sub (T.Var "x") (T.Literal 1)))
        )
    )

prog2 :: T.Term
prog2 = T.Seq (T.Write (T.Var "x")) (T.Write (T.Literal 42))
