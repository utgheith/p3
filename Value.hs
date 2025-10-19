module Value
  ( Value (..),
    valueToInt,
    valueToBool,
    valueToString,
    valueToTuple,
    isIntVal,
    isBoolVal,
    isStringVal,
    isListVal,
    extractInt,
    extractBool,
    extractString,
    extractList,
  )
where

import Term (Term)

data Value
  = IntVal Integer
  | BoolVal Bool
  | ListVal [Value]
  | StringVal String
  | Tuple [Value]
  | ClosureVal [String] Term [(String, Value)]
  deriving (Eq, Show)

valueToInt :: Value -> Either String Integer
valueToInt (IntVal n) = Right n
valueToInt (BoolVal _) = Left "Type error: expected integer, got boolean"
valueToInt (StringVal _) = Left "Type error: expected integer, got string"
valueToInt (ListVal _) = Left "Type error: expected integer, got list"
valueToInt (Tuple _) = Left "Type error: expected integer, got tuple"
valueToInt (ClosureVal {}) = Left "Type error: expected integer, got function"

valueToBool :: Value -> Either String Bool
valueToBool (BoolVal b) = Right b
valueToBool (IntVal _) = Left "Type error: expected boolean, got integer"
valueToBool (StringVal _) = Left "Type error: expected boolean, got string"
valueToBool (ListVal _) = Left "Type error: expected boolean, got list"
valueToBool (Tuple _) = Left "Type error: expected boolean, got tuple"
valueToBool (ClosureVal {}) = Left "Type error: expected boolean, got function"

valueToString :: Value -> Either String String
valueToString (StringVal s) = Right s
valueToString (IntVal _) = Left "Type error: expected string, got integer"
valueToString (BoolVal _) = Left "Type error: expected string, got boolean"
valueToString (ListVal _) = Left "Type error: expected string, got list"
valueToString (Tuple _) = Left "Type error: expected string, got tuple"
valueToString (ClosureVal {}) = Left "Type error: expected string, got function"

valueToTuple :: Value -> Either String [Value]
valueToTuple (Tuple s) = Right s
valueToTuple (IntVal _) = Left "Type error: expected tuple, got integer"
valueToTuple (BoolVal _) = Left "Type error: expected tuple, got boolean"
valueToTuple (StringVal _) = Left "Type error: expected tuple, got string"
valueToTuple (ClosureVal {}) = Left "Type error: expected tuple, got function"

isIntVal :: Value -> Bool
isIntVal (IntVal _) = True
isIntVal _ = False

isBoolVal :: Value -> Bool
isBoolVal (BoolVal _) = True
isBoolVal _ = False

isStringVal :: Value -> Bool
isStringVal (StringVal _) = True
isStringVal _ = False

isListVal :: Value -> Bool
isListVal (ListVal _) = True
isListVal _ = False

extractInt :: Value -> Integer
extractInt (IntVal n) = n
extractInt _ = error "extractInt: not an IntVal"

extractBool :: Value -> Bool
extractBool (BoolVal b) = b
extractBool _ = error "extractBool: not a BoolVal"

extractString :: Value -> String
extractString (StringVal s) = s
extractString _ = error "extractString: not a StringVal"

extractList :: Value -> [Value]
extractList (ListVal xs) = xs
extractList _ = error "extractList: not a ListVal"
