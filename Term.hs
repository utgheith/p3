module Term(Term(..), BinaryOp(..)) where

data BinaryOp = Add | Sub | Mul | Div | Mod
    deriving (Eq, Show)

data Term = If Term Term Term
          | Let String Term
          | Literal Integer
          | Read String
          | Seq Term Term
          | Skip
          | BinaryOps BinaryOp Term Term
          | Var String
          | While Term Term
          | Write Term
          deriving (Eq, Show)
