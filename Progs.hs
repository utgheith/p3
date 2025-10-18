module Progs (
    prog
  , prog2
  , prog3
  , progLiteral
  , progVar
  , progLet
  , progSeq
  , progIf
  , progWhile
  , progRead
  , progCompare
  , progLogic
  , progBinaryOps
  , progWrite
  , progSkip
  , progFull
  ) where

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
    ~ Write (Gt (Var "x") (Var "y"))
    ~ Write (Lt (Var "x") (Var "y"))
    ~ Write (Eq (Var "x") (Var "y"))
    ~ Write (Neq (Var "x") (Var "y"))
    ~ Write (And (Gt (Var "x") (Var "y")) (Neq (Var "x") (Var "y")))
    ~ Write (Or (Lt (Var "x") (Var "y")) (Gt (Var "x") (Var "y")))
    ~ Write (Not (Lt (Var "x") (Var "y")))
    ~ If (Gt (Var "x") (Var "y")) (Write (Literal 100)) (Write (Literal 200))
    ~ While
      (Gt (Var "y") (Literal 0))
      ( Write (Var "y")
          ~ "y" <=> BinaryOps Sub (Var "y") (Literal 1)
      )


