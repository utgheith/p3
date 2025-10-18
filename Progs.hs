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


-- Test each small-step reduction rule individually


-- 1. Literal
progLiteral :: Term
progLiteral = Write (Literal 42)


-- 2. Var
progVar :: Term
progVar =
  "x" <=> Literal 7
    ~ Write (Var "x")


-- 3. Let
progLet :: Term
progLet =
  "x" <=> BinaryOps Add (Literal 2) (Literal 3)
    ~ Write (Var "x")


-- 4. Seq
progSeq :: Term
progSeq = Write (Literal 1) ~ Write (Literal 2)


-- 5. If
progIf :: Term
progIf =
  If (Gt (Literal 5) (Literal 3))
    (Write (Literal 100))
    (Write (Literal 200))


-- 6. While
progWhile :: Term
progWhile =
  "x" <=> Literal 3
    ~ While
      (Gt (Var "x") (Literal 0))
      ( Write (Var "x")
          ~ "x" <=> BinaryOps Sub (Var "x") (Literal 1)
      )


-- 7. Read
progRead :: Term
progRead = Read "x" ~ Write (Var "x")


-- 8. Comparison ops
progCompare :: Term
progCompare =
  "x" <=> Literal 10
    ~ "y" <=> Literal 5
    ~ Write (Lt (Var "y") (Var "x"))
    ~ Write (Gt (Var "x") (Var "y"))
    ~ Write (Lte (Var "y") (Var "x"))
    ~ Write (Gte (Var "x") (Var "y"))
    ~ Write (Eq (Var "x") (Var "y"))
    ~ Write (Neq (Var "x") (Literal 42))


-- 9. Logical ops
progLogic :: Term
progLogic =
  "a" <=> Literal 1
    ~ "b" <=> Literal 0
    ~ Write (And (Var "a") (Var "b"))
    ~ Write (Or (Var "a") (Var "b"))
    ~ Write (Not (Var "b"))


-- 10. BinaryOps (arithmetic)
progBinaryOps :: Term
progBinaryOps =
  "x" <=> Literal 6
    ~ "y" <=> Literal 2
    ~ Write (BinaryOps Add (Var "x") (Var "y"))
    ~ Write (BinaryOps Sub (Var "x") (Var "y"))
    ~ Write (BinaryOps Mul (Var "x") (Var "y"))
    ~ Write (BinaryOps Div (Var "x") (Var "y"))
    ~ Write (BinaryOps Mod (Var "x") (Var "y"))


-- 11. Write
progWrite :: Term
progWrite = Write (Literal 999)


-- 12. Skip
progSkip :: Term
progSkip = Skip


-- 13. Full integration test (combination)
progFull :: Term
progFull =
  "x" <=> Literal 4
    ~ "y" <=> Literal 2
    ~ If (Gt (Var "x") (Var "y"))
        (Write (Literal 111))
        (Write (Literal 222))
    ~ While
        (Gt (Var "x") (Literal 0))
        ( Write (Var "x")
            ~ "x" <=> BinaryOps Sub (Var "x") (Literal 1)
        )
