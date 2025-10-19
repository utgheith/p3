module Value
  ( Value (..),
    valueToInt,
    valueToBool,
    valueToString,
    valueToTuple,
    valueToList,
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
  | StringVal String
  | Tuple [Value]
  | List [Value]
  | ClosureVal [String] Term [(String, Value)]
  deriving (Eq, Show)

valueToInt :: Value -> Either String Integer
valueToInt (IntVal n) = Right n
valueToInt (BoolVal _) = Left "Type error: expected integer, got boolean"
valueToInt (StringVal _) = Left "Type error: expected integer, got string"
valueToInt (Tuple _) = Left "Type error: expected integer, got tuple"
valueToInt (List _) = Left "Type error: expected integer, got list"
valueToInt (ClosureVal {}) = Left "Type error: expected integer, got function"

valueToBool :: Value -> Either String Bool
valueToBool (BoolVal b) = Right b
valueToBool (IntVal _) = Left "Type error: expected boolean, got integer"
valueToBool (StringVal _) = Left "Type error: expected boolean, got string"
valueToBool (Tuple _) = Left "Type error: expected boolean, got tuple"
valueToBool (List _) = Left "Type error: expected boolean, got list"
valueToBool (ClosureVal {}) = Left "Type error: expected boolean, got function"

valueToString :: Value -> Either String String
valueToString (StringVal s) = Right s
valueToString (IntVal _) = Left "Type error: expected string, got integer"
valueToString (BoolVal _) = Left "Type error: expected string, got boolean"
valueToString (Tuple _) = Left "Type error: expected string, got tuple"
valueToString (List _) = Left "Type error: expected string, got list"
valueToString (ClosureVal {}) = Left "Type error: expected string, got function"

valueToTuple :: Value -> Either String [Value]
valueToTuple (Tuple s) = Right s
valueToTuple (IntVal _) = Left "Type error: expected tuple, got integer"
valueToTuple (BoolVal _) = Left "Type error: expected tuple, got boolean"
valueToTuple (StringVal _) = Left "Type error: expected tuple, got string"
valueToTuple (ClosureVal {}) = Left "Type error: expected tuple, got function"
valueToTuple (List _) = Left "Type error: expected tuple, got list"

valueToList :: Value -> Either String [Value]
valueToList (List s) = Right s
valueToList (IntVal _) = Left "Type error: expected list, got integer"
valueToList (BoolVal _) = Left "Type error: expected list, got boolean"
valueToList (StringVal _) = Left "Type error: expected list, got string"
valueToList (Tuple _) = Left "Type error: expected list, got tuple"
valueToList (ClosureVal {}) = Left "Type error: expected list, got function"

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
isListVal (List _) = True
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
extractList (List s) = s
extractList _ = error "extractList: not a List"
