{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Term (Term, Ref, TermG (..), TermPosition (..), BinaryOp (..), UnaryOp (..), ErrorKind (..), ErrorKindOrAny (..)) where

data BinaryOp = Add | Sub | Mul | Div | Mod | Lt | Gt | Lte | Gte | Eq | Neq | And | Or | Pow | Xor
  deriving (Eq, Show)

data UnaryOp = Neg | Not | BitNot
  deriving (Eq, Show)

data ErrorKind = Arithmetic | Type | Input | VariableNotFound | Arguments deriving (Eq, Show)

data ErrorKindOrAny = Specific ErrorKind | Any deriving (Eq, Show)

data TermPosition = Both | RValue deriving (Eq, Show)

data TermG (p :: TermPosition) where
  If :: TermG 'RValue -> TermG 'RValue -> TermG 'RValue -> TermG 'RValue
  Try :: TermG 'RValue -> ErrorKindOrAny -> TermG 'RValue -> TermG 'RValue
  Let :: TermG 'Both -> TermG 'RValue -> TermG 'RValue
  Literal :: Integer -> TermG 'RValue
  StringLiteral :: String -> TermG 'RValue
  Read :: String -> TermG 'RValue
  Seq :: TermG 'RValue -> TermG 'RValue -> TermG 'RValue
  Skip :: TermG 'RValue
  BinaryOps :: BinaryOp -> TermG 'RValue -> TermG 'RValue -> TermG 'RValue
  UnaryOps :: UnaryOp -> TermG 'RValue -> TermG 'RValue
  Var :: TermG 'Both -> TermG 'RValue
  While :: TermG 'RValue -> TermG 'RValue -> TermG 'RValue
  Write :: TermG 'RValue -> TermG 'RValue
  BoolLit :: Bool -> TermG 'RValue
  TupleTerm :: [TermG 'RValue] -> TermG 'RValue
  NewDictionary :: TermG 'RValue
  Retrieve :: TermG 'RValue -> TermG 'RValue -> TermG 'RValue
  Merge :: TermG 'RValue -> TermG 'RValue -> TermG 'RValue -> TermG 'RValue
  Fun :: [String] -> TermG 'RValue -> TermG 'RValue
  ApplyFun :: TermG 'RValue -> [TermG 'RValue] -> TermG 'RValue
  PreIncrement :: String -> TermG 'RValue
  PreDecrement :: String -> TermG 'RValue
  PostIncrement :: String -> TermG 'RValue
  PostDecrement :: String -> TermG 'RValue
  BreakSignal :: TermG 'RValue
  ContinueSignal :: TermG 'RValue
  OnlyStr :: String -> TermG 'Both
  Bracket :: TermG 'Both -> TermG 'RValue -> TermG 'Both

deriving instance Show (TermG p)

deriving instance Eq (TermG p)

type Term = TermG 'RValue

type Ref = TermG 'Both
