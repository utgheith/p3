module Value
  ( Value (..),
    valueToInt,
    valueToBool,
    valueToString,
    isIntVal,
    isBoolVal,
    isStringVal,
    extractInt,
    extractBool,
    extractString,
  )
where

data Value
  = IntVal Integer
  | BoolVal Bool
  | StringVal String
  deriving (Eq, Show)

valueToInt :: Value -> Either String Integer
valueToInt (IntVal n) = Right n
valueToInt (BoolVal _) = Left "Type error: expected integer, got boolean"
valueToInt (StringVal _) = Left "Type error: expected integer, got string"

valueToBool :: Value -> Either String Bool
valueToBool (BoolVal b) = Right b
valueToBool (IntVal _) = Left "Type error: expected boolean, got integer"
valueToBool (StringVal _) = Left "Type error: expected boolean, got string"

valueToString :: Value -> Either String String
valueToString (StringVal s) = Right s
valueToString (IntVal _) = Left "Type error: expected string, got integer"
valueToString (BoolVal _) = Left "Type error: expected string, got boolean"

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
