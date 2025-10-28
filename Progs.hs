module Progs (prog, prog2, prog3) where

import Term
import TypeSignature (TypeSignature (..))

infixl 1 ~

(~) :: Term -> Term -> Term
(~) = Seq

infixl 9 <=>

(<=>) :: Term -> Term -> Term
(<=>) = Let

x :: Term
x = OnlyStr ("x", TUnknown)

y :: Term
y = OnlyStr ("y", TUnknown)

prog :: Term
prog =
  x <=> Literal 10
    ~ While
      (Var x)
      ( Write (Var x)
          ~ x <=> BinaryOps Sub (Var x) (Literal 1)
      )
      Nothing
      Nothing

prog2 :: Term
prog2 = Write (Var x) ~ Write (Literal 42)

prog3 :: Term
prog3 =
  x <=> Literal 10
    ~ y <=> Literal 5
    ~ Write (BinaryOps Gt (Var x) (Var y))
    ~ Write (BinaryOps Lt (Var x) (Var y))
    ~ Write (BinaryOps Eq (Var x) (Var y))
    ~ Write (BinaryOps Neq (Var x) (Var y))
    ~ Write (BinaryOps And (BinaryOps Gt (Var x) (Var y)) (BinaryOps Neq (Var x) (Var y)))
    ~ Write (BinaryOps Or (BinaryOps Lt (Var x) (Var y)) (BinaryOps Gt (Var x) (Var y)))
    ~ Write (UnaryOps Not (BinaryOps Lt (Var x) (Var y)))
    ~ If (BinaryOps Gt (Var x) (Var y)) (Write (Literal 100)) (Write (Literal 200))
    ~ While
      (BinaryOps Gt (Var y) (Literal 0))
      ( Write (Var y)
          ~ y <=> BinaryOps Sub (Var y) (Literal 1)
      )
      Nothing
      Nothing
