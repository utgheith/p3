module TypeSignature (TypedName, TypeSignature (..)) where

data TypeSignature
  = TUnit
  | TInt
  | TBool
  | TString
  | TTuple [TypeSignature]
  | TFun [TypeSignature] TypeSignature
  | TSum [TypeSignature]
  | TDictionary TypeSignature
  | TUnknown
  deriving (Eq, Show)

type TypedName = (String, TypeSignature)
