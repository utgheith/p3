module Term (Ref (..), Term (..), BinaryOp (..), UnaryOp (..), ErrorKind (..), ErrorKindOrAny (..)) where

data BinaryOp = Add | Sub | Mul | Div | Mod | Lt | Gt | Lte | Gte | Eq | Neq | And | Or | Pow | Xor
  deriving (Eq, Show)

data UnaryOp = Neg | Not | BitNot
  deriving (Eq, Show)

data ErrorKind = Arithmetic | Type | Input | VariableNotFound | Arguments deriving (Eq, Show)

data ErrorKindOrAny = Specific ErrorKind | Any deriving (Eq, Show)

data Ref = OnlyStr String | Bracket Ref Term deriving (Eq, Show)

data Term
  = If Term Term Term
  | Try Term ErrorKindOrAny Term
  | Let Ref Term
  | Literal Integer
  | StringLiteral String
  | Read String
  | Seq Term Term
  | Skip
  | BinaryOps BinaryOp Term Term
  | UnaryOps UnaryOp Term
  | Var Ref
  | While Term Term
  | Write Term
  | BoolLit Bool
  | TupleTerm [Term]
  | NewDictionary
  | Retrieve Term Term
  | Merge Term Term Term -- current value, index, value
  | Fun [String] Term
  | ApplyFun Term [Term]
  | PreIncrement String
  | PreDecrement String
  | PostIncrement String
  | PostDecrement String
  | BreakSignal
  | ContinueSignal
  | ReturnExp Term
  deriving (Eq, Show)
