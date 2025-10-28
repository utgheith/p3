module TypeError (TypeError (..)) where

import Term (BinaryOp, UnaryOp)
import TypeSignature (TypeSignature)

data TypeError
  = BinaryOpError BinaryOp TypeSignature TypeSignature
  | UnaryOpError UnaryOp TypeSignature
  | NotAFunction TypeSignature
  | ArityMismatch Int Int
  | ArgumentTypeMismatch Int TypeSignature TypeSignature
  | NotIndexable TypeSignature
  | InvalidIndexType TypeSignature TypeSignature
  | ConditionNotBool TypeSignature
  | ForRangeNotInt TypeSignature TypeSignature
  deriving (Eq, Show)
