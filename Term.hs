module Term (Term (..), BinaryOp (..), UnaryOp (..)) where

data BinaryOp = Add | Sub | Mul | Div | Mod | Lt | Gt | Lte | Gte | Eq | Neq | And | Or
  deriving (Eq, Show)

data UnaryOp = Neg | Not | Head | Tail | IsNil | Length
  deriving (Eq, Show)

data Term
  = If Term Term Term
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
  | Nil
  | ConsTerm Term Term
  | TupleTerm [Term]
  | NewDictionary
  | AccessBracket Term Term
  | SetBracket String Term Term
  | Fun [String] Term
  | ApplyFun Term [Term]
  | BreakSignal
  | ContinueSignal
  deriving (Eq, Show)
