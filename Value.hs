module Value
  ( Value (..),
    valueToInt,
    valueToBool,
    valueToString,
    valueToTuple,
    isIntVal,
    isBoolVal,
    isStringVal,
    extractInt,
    extractBool,
    extractString,
  )
where

import qualified Data.Map as M
import Term (Term)

data Value
  = IntVal Integer
  | BoolVal Bool
  | StringVal String
  | Tuple [Value]
  | ClosureVal [String] Term [(String, Value)]
  | Dictionary (M.Map Integer Value)
  deriving (Eq, Show)

valueToDebugString :: Value -> String
valueToDebugString(IntVal _) = "Integer"
valueToDebugString(BoolVal _) = "Boolean"
valueToDebugString(StringVal _) = "String"
valueToDebugString(Tuple _) = "Tuple"
valueToDebugString(ClosureVal {}) = "Function"
valueToDebugString(Dictionary _) = "Dictionary"

valueToInt :: Value -> Either String Integer
valueToInt (IntVal n) = Right n
valueToInt x = Left $ "Type error: expected integer, got " ++ valueToDebugString x

valueToBool :: Value -> Either String Bool
valueToBool (BoolVal b) = Right b
valueToBool x = Left $ "Type error: expected boolean, got " ++ valueToDebugString x

valueToString :: Value -> Either String String
valueToString (StringVal s) = Right s
valueToString x = Left $ "Type error: expected string, got " ++ valueToDebugString x

valueToTuple :: Value -> Either String [Value]
valueToTuple (Tuple s) = Right s
valueToTuple x = Left $ "Type error: expected tuple, got " ++ valueToDebugString x

isIntVal :: Value -> Bool
isIntVal (IntVal _) = True
isIntVal _ = False

isBoolVal :: Value -> Bool
isBoolVal (BoolVal _) = True
isBoolVal _ = False

isStringVal :: Value -> Bool
isStringVal (StringVal _) = True
isStringVal _ = False

extractInt :: Value -> Integer
extractInt (IntVal n) = n
extractInt _ = error "extractInt: not an IntVal"

extractBool :: Value -> Bool
extractBool (BoolVal b) = b
extractBool _ = error "extractBool: not a BoolVal"

extractString :: Value -> String
extractString (StringVal s) = s
extractString _ = error "extractString: not a StringVal"
