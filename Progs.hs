module Progs (prog, prog2, prog3, prog4, prog5) where

import Term

infixl 1 ~

(~) :: Term -> Term -> Term
(~) = Seq

infixl 9 <=>

(<=>) :: String -> Term -> Term
(<=>) = Let

prog :: Term
prog =
  "x" <=> Literal 10
    ~ While
      (Var "x")
      ( Write (Var "x")
          ~ "x" <=> BinaryOps Sub (Var "x") (Literal 1)
      )

prog2 :: Term
prog2 = Write (Var "x") ~ Write (Literal 42)

prog3 :: Term
prog3 =
  "x" <=> Literal 10
    ~ "y" <=> Literal 5
    ~ Write (BinaryOps Gt (Var "x") (Var "y"))
    ~ Write (BinaryOps Lt (Var "x") (Var "y"))
    ~ Write (BinaryOps Eq (Var "x") (Var "y"))
    ~ Write (BinaryOps Neq (Var "x") (Var "y"))
    ~ Write (BinaryOps And (BinaryOps Gt (Var "x") (Var "y")) (BinaryOps Neq (Var "x") (Var "y")))
    ~ Write (BinaryOps Or (BinaryOps Lt (Var "x") (Var "y")) (BinaryOps Gt (Var "x") (Var "y")))
    ~ Write (UnaryOps Not (BinaryOps Lt (Var "x") (Var "y")))
    ~ If (BinaryOps Gt (Var "x") (Var "y")) (Write (Literal 100)) (Write (Literal 200))
    ~ While
      (BinaryOps Gt (Var "y") (Literal 0))
      ( Write (Var "y")
          ~ "y" <=> BinaryOps Sub (Var "y") (Literal 1)
      )

prog4 :: Term
prog4 =
  ConcurSeq (Let "x" (Literal 2)) (Let "y" (Literal 3))

prog5 :: Term
prog5 =
  let addop = BinaryOps Add (Var "x") (Literal 1)
   in Seq
        (Let "x" (Literal 0))
        ( ConcurSeq
            (Let "x" (If (Literal 1) (addop) (addop)))
            (Let "x" (If (Literal 1) (addop) (addop)))
        )
