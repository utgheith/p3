module Progs (prog, prog2, prog3) where

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

prog3 :: T.Term
prog3 =
  T.Seq
    (T.Let "x" (T.Literal 10))
    ( T.Seq
        (T.Let "y" (T.Literal 5))
        ( T.Seq
            (T.Write (T.Gt (T.Var "x") (T.Var "y")))
            ( T.Seq
                (T.Write (T.Lt (T.Var "x") (T.Var "y")))
                ( T.Seq
                    (T.Write (T.Eq (T.Var "x") (T.Var "y")))
                    ( T.Seq
                        (T.Write (T.Neq (T.Var "x") (T.Var "y")))
                        ( T.Seq
                            (T.Write (T.And (T.Gt (T.Var "x") (T.Var "y")) (T.Neq (T.Var "x") (T.Var "y"))))
                            ( T.Seq
                                (T.Write (T.Or (T.Lt (T.Var "x") (T.Var "y")) (T.Gt (T.Var "x") (T.Var "y"))))
                                ( T.Seq
                                    (T.Write (T.Not (T.Lt (T.Var "x") (T.Var "y"))))
                                    ( T.Seq
                                        (T.If (T.Gt (T.Var "x") (T.Var "y")) (T.Write (T.Literal 100)) (T.Write (T.Literal 200)))
                                        ( T.While
                                            (T.Gt (T.Var "y") (T.Literal 0))
                                            ( T.Seq
                                                (T.Write (T.Var "y"))
                                                (T.Let "y" (T.Sub (T.Var "y") (T.Literal 1)))
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
