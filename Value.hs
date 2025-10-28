module Value
  ( Value (..),
    Scope (..),
    lookupScope,
    insertScope,
    getAllBindings,
    emptyScope,
    scopeFromList,
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
import TypeSignature (TypedName)

-- Scope definition (moved here to avoid circular dependency with Value)
data Scope = Scope (M.Map String Value) (Maybe Scope)
  deriving (Eq, Show)

lookupScope :: String -> Scope -> Maybe Value
lookupScope name (Scope m parent) =
  case M.lookup name m of
    Just v -> Just v
    Nothing -> case parent of
      Just p -> lookupScope name p
      Nothing -> Nothing

insertScope :: String -> Value -> Scope -> Scope
insertScope name val (Scope m parent) = Scope (M.insert name val m) parent

getAllBindings :: Scope -> [(String, Value)]
getAllBindings scope = addAll scope []
  where
    addAll (Scope m Nothing) vars = M.toList m ++ vars
    addAll (Scope m (Just parent)) vars = addAll parent (M.toList m ++ vars)

emptyScope :: Scope
emptyScope = Scope M.empty Nothing

scopeFromList :: [(String, Value)] -> Scope
scopeFromList vars = Scope (M.fromList vars) Nothing

-- Value definition
data Value
  = IntVal Integer
  | BoolVal Bool
  | StringVal String
  | Tuple [Value]
  | ClosureVal [TypedName] Term Scope
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
