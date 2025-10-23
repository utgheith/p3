module Progs (prog, prog2, prog3) where

import Term

infixl 1 ~

(~) :: Term -> Term -> Term
(~) = Seq

infixl 9 <=>

(<=>) :: Term -> Term -> Term
(<=>) = Let

prog :: Term
prog =
  OnlyStr "x" <=> Literal 10
    ~ While
      (Var (OnlyStr "x"))
      ( Write (Var (OnlyStr "x"))
          ~ OnlyStr "x" <=> BinaryOps Sub (Var (OnlyStr "x")) (Literal 1)
      )

prog2 :: Term
prog2 = Write (Var (OnlyStr "x")) ~ Write (Literal 42)

prog3 :: Term
prog3 =
  OnlyStr "x" <=> Literal 10
    ~ OnlyStr "y" <=> Literal 5
    ~ Write (BinaryOps Gt (Var (OnlyStr "x")) (Var (OnlyStr "y")))
    ~ Write (BinaryOps Lt (Var (OnlyStr "x")) (Var (OnlyStr "y")))
    ~ Write (BinaryOps Eq (Var (OnlyStr "x")) (Var (OnlyStr "y")))
    ~ Write (BinaryOps Neq (Var (OnlyStr "x")) (Var (OnlyStr "y")))
    ~ Write (BinaryOps And (BinaryOps Gt (Var (OnlyStr "x")) (Var (OnlyStr "y"))) (BinaryOps Neq (Var (OnlyStr "x")) (Var (OnlyStr "y"))))
    ~ Write (BinaryOps Or (BinaryOps Lt (Var (OnlyStr "x")) (Var (OnlyStr "y"))) (BinaryOps Gt (Var (OnlyStr "x")) (Var (OnlyStr "y"))))
    ~ Write (UnaryOps Not (BinaryOps Lt (Var (OnlyStr "x")) (Var (OnlyStr "y"))))
    ~ If (BinaryOps Gt (Var (OnlyStr "x")) (Var (OnlyStr "y"))) (Write (Literal 100)) (Write (Literal 200))
    ~ While
      (BinaryOps Gt (Var (OnlyStr "y")) (Literal 0))
      ( Write (Var (OnlyStr "y"))
          ~ OnlyStr "y" <=> BinaryOps Sub (Var (OnlyStr "y")) (Literal 1)
      )
