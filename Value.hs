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
import Sprintf ((%))

data Value
  = IntVal Integer
  | BoolVal Bool
  | StringVal String
  | Tuple [Value]
  | ClosureVal [String] Term [(String, Value)]
  | Dictionary (M.Map Integer Value)
  deriving (Eq, Show)

data Type
  = IntType
  | BoolType
  | StringType
  | TupleType
  | ClosureType
  | DictionaryType

typeOf :: Value -> Type
typeOf (IntVal _) = IntType
typeOf (BoolVal _) = BoolType
typeOf (StringVal _) = StringType
typeOf (Tuple _) = TupleType
typeOf (ClosureVal {}) = ClosureType
typeOf (Dictionary _) = DictionaryType

typeName :: Type -> String
typeName IntType = "integer"
typeName BoolType = "boolean"
typeName StringType = "string"
typeName TupleType = "tuple"
typeName ClosureType = "function"
typeName DictionaryType = "dictionary"

typeError :: Type -> Value -> Either String t
typeError expected actual = Left $ "Type error: expected %s, got %s" % map typeName [expected, typeOf actual]

valueToInt :: Value -> Either String Integer
valueToInt (IntVal n) = Right n
valueToInt other = typeError IntType other

valueToBool :: Value -> Either String Bool
valueToBool (BoolVal b) = Right b
valueToBool other = typeError BoolType other

valueToString :: Value -> Either String String
valueToString (StringVal s) = Right s
valueToString other = typeError StringType other

valueToTuple :: Value -> Either String [Value]
valueToTuple (Tuple s) = Right s
valueToTuple other = typeError TupleType other

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
