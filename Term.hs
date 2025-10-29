{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Term (Term (..), TermF (..), BinaryOp (..), UnaryOp (..), ErrorKind (..), ErrorKindOrAny (..)) where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import TypeSignature (TypedName)

data BinaryOp = Add | Sub | Mul | Div | Mod | Lt | Gt | Lte | Gte | Eq | Neq | And | Or | Pow | Xor
  deriving (Eq, Show)

data UnaryOp = Neg | Not | BitNot
  deriving (Eq, Show)

data ErrorKind = Assertion | Arithmetic | Type | Input | VariableNotFound | Arguments | Internal deriving (Eq, Show)

data ErrorKindOrAny = Specific ErrorKind | Any deriving (Eq, Show)

data Term
  = If Term Term Term
  | Try Term ErrorKindOrAny Term
  | Let Term Term
  | Literal Integer
  | StringLiteral String
  | Read TypedName
  | Seq Term Term
  | Skip
  | BinaryOps BinaryOp Term Term
  | UnaryOps UnaryOp Term
  | Var Term
  | OnlyStr TypedName
  | Bracket Term Term
  | While Term Term (Maybe Term) (Maybe Term)
  | WhileBody Term Term Term (Maybe Term) (Maybe Term)
  | For TypedName Term Term Term (Maybe Term) (Maybe Term)
  | Write Term
  | BoolLit Bool
  | TupleTerm [Term]
  | NewDictionary
  | Retrieve Term Term
  | Merge Term Term Term -- current value, index, value
  | Fun [TypedName] Term
  | ApplyFun Term [Term]
  | PreIncrement TypedName
  | PreDecrement TypedName
  | PostIncrement TypedName
  | PostDecrement TypedName
  | BreakSignal
  | ContinueSignal
  | Assert Term
  deriving (Eq, Show)

makeBaseFunctor ''Term
