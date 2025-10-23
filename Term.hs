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
  If :: Term -> Term -> Term -> Term
  Try :: Term -> ErrorKindOrAny -> Term -> Term
  Let :: Ref -> Term -> Term
  Literal :: Integer -> Term
  StringLiteral :: String -> Term
  Read :: String -> Term
  Seq :: Term -> Term -> Term
  Skip :: Term
  BinaryOps :: BinaryOp -> Term -> Term -> Term
  UnaryOps :: UnaryOp -> Term -> Term
  Var :: Ref -> Term
  While :: Term -> Term -> Term
  Write :: Term -> Term
  BoolLit :: Bool -> Term
  TupleTerm :: [Term] -> Term
  NewDictionary :: Term
  Retrieve :: Term -> Term -> Term
  Merge :: Term -> Term -> Term -> Term
  Fun :: [String] -> Term -> Term
  ApplyFun :: Term -> [Term] -> Term
  PreIncrement :: String -> Term
  PreDecrement :: String -> Term
  PostIncrement :: String -> Term
  PostDecrement :: String -> Term
  BreakSignal :: Term
  ContinueSignal :: Term
  OnlyStr :: String -> Ref
  Bracket :: Ref -> Term -> Ref

deriving instance Show (TermG p)

deriving instance Eq (TermG p)

type Term = TermG 'RValue

type Ref = TermG 'Both
