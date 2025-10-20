module Term (Term (..), BinaryOp (..), UnaryOp (..), ErrorKind (..), ErrorKindOrAny (..)) where

data BinaryOp = Add | Sub | Mul | Div | Mod | Lt | Gt | Lte | Gte | Eq | Neq | And | Or
  deriving (Eq, Show)

data UnaryOp = Neg | Not
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
  | UnaryOps UnaryOp Term
  | Var String
  | While Term Term
  | Write Term
  | BoolLit Bool
  | TupleTerm [Term]
  | NewDictionary
  | AccessBracket Term Term
  | SetBracket String Term Term
  | Fun [String] Term
  | ApplyFun Term [Term]
  deriving (Eq, Show)
