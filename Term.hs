module Term (Term (..), BinaryOp (..), ErrorKind(..), ErrorKindOrAny(..)) where

data BinaryOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Show)

data ErrorKind = Arithmetic | Type | Input | VariableNotFound deriving (Eq, Show)
data ErrorKindOrAny = Specific ErrorKind | Any deriving (Eq, Show)

data Term
  = If Term Term Term
  | Try Term ErrorKindOrAny Term
  | Let String Term
  | Literal Integer
  | StringLiteral String
  | Read String
  | Seq Term Term
  | Skip
  | BinaryOps BinaryOp Term Term
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
