module Term(Term(..)) where

data Term = If Term Term Term
          | Let String Term
          | Literal Integer
          | Read String
          | Seq Term Term
          | Skip
          | Sub Term Term
          | Add Term Term
          | Mul Term Term
          | Div Term Term
          | Var String
          | While Term Term
          | Write Term
          deriving (Eq, Show)
