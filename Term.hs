module Term (Term (..), BinaryOp (..), UnaryOp (..)) where

data BinaryOp = Add | Sub | Mul | Div | Mod | Lt | Gt | Lte | Gte | Eq | Neq | And | Or
  deriving (Eq, Show)

data UnaryOp = Neg | Not
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
  | TupleTerm [Term]
  | NewDictionary
  | AccessBracket Term Term
  | SetBracket String Term Term
  | Fun [String] Term
  | ApplyFun Term [Term]
  | BreakSignal
  | ContinueSignal
  | ClassDef String Term Term Term -- class name, constructor, instance vars, methods
  | ClassInstantiate String [Term] -- class name, constructor args
--   | ClassVar String String -- class name, variable name
--   | ClassMethodCall String String [Term] -- class name, method name, args
  deriving (Eq, Show)
