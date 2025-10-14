module Term(Term(..)) where

data Term = If Term Term Term
          | Let String Term
          | Literal Integer
          | Read String
          | Seq Term Term
          | Skip
          | Sub Term Term
          | Var String
          | While Term Term
          | Write Term
          | BoolLit Bool
          | Lt Term Term
          | Gt Term Term
          | Lte Term Term
          | Gte Term Term
          | Eq Term Term
          | Neq Term Term
          | And Term Term
          | Or Term Term
          | Not Term
          deriving (Eq, Show)
