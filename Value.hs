module Value
  ( Value (..),
    valueToInt,
    valueToBool,
    valueToString,
    valueToTuple,
    isIntVal,
    isBoolVal,
    isStringVal,
  )
where

import qualified Data.Map as M
import Sprintf ((%))
import Term (Term)

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
  deriving (Eq)

typeOf :: Value -> Type
typeOf (IntVal _) = IntType
typeOf (BoolVal _) = BoolType
typeOf (StringVal _) = StringType
typeOf (Tuple _) = TupleType
typeOf ClosureVal {} = ClosureType
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
isIntVal = (==) IntType . typeOf

isBoolVal :: Value -> Bool
isBoolVal = (==) BoolType . typeOf

isStringVal :: Value -> Bool
isStringVal = (==) StringType . typeOf
