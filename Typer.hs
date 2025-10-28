{-# LANGUAGE DataKinds #-}

module Typer (typer) where

import Data.Foldable (traverse_)
import Data.Functor.Foldable (para)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Term (BinaryOp (..), Term (..), TermF (..), UnaryOp (..))
import TypeError (TypeError (..))
import TypeSignature (TypeSignature (..))

-- Type environment maps variable names to their types
type TypeEnv = M.Map String TypeSignature

-- Result is a function from environment to Either error or type
type TypeReader = TypeEnv -> Either TypeError TypeSignature

-- Union type vs. Sum type
combine :: TypeSignature -> TypeSignature -> TypeSignature
combine TUnknown _ = TUnknown
combine _ TUnknown = TUnknown
combine t1 t2 | t1 == t2 = t1
combine t1 t2 = TSum [t1, t2]

-- Helper: Check if a binary operation is valid for given types
checkBinaryOp :: BinaryOp -> TypeSignature -> TypeSignature -> Either TypeError TypeSignature
checkBinaryOp op t1 t2 = case op of
  Add | t1 == TInt && t2 == TInt -> Right TInt
  Sub | t1 == TInt && t2 == TInt -> Right TInt
  Mul | t1 == TInt && t2 == TInt -> Right TInt
  Div | t1 == TInt && t2 == TInt -> Right TInt
  Mod | t1 == TInt && t2 == TInt -> Right TInt
  Pow | t1 == TInt && t2 == TInt -> Right TInt
  Lt | t1 == TInt && t2 == TInt -> Right TBool
  Gt | t1 == TInt && t2 == TInt -> Right TBool
  Lte | t1 == TInt && t2 == TInt -> Right TBool
  Gte | t1 == TInt && t2 == TInt -> Right TBool
  Eq | t1 == t2 && t1 /= TUnknown -> Right TBool
  Neq | t1 == t2 && t1 /= TUnknown -> Right TBool
  And | t1 == TBool && t2 == TBool -> Right TBool
  Or | t1 == TBool && t2 == TBool -> Right TBool
  Xor | t1 == TBool && t2 == TBool -> Right TBool
  _ -> Left $ BinaryOpError op t1 t2

-- Helper: Check if a unary operation is valid for given type
checkUnaryOp :: UnaryOp -> TypeSignature -> Either TypeError TypeSignature
checkUnaryOp op t = case (op, t) of
  (Not, TBool) -> Right TBool
  (Neg, TInt) -> Right TInt
  (BitNot, TInt) -> Right TInt
  _ -> Left $ UnaryOpError op t

-- Helper: Check if a type can be used in a boolean context (truthiness)
isTruthy :: TypeSignature -> Bool
isTruthy TBool = True
isTruthy TInt = True
isTruthy TString = True
isTruthy (TTuple _) = True
isTruthy TUnit = True -- empty tuple () is truthy
isTruthy (TSum ts) = all isTruthy ts -- truthy if all variants are truthy
isTruthy TUnknown = True -- conservative: allow unknown types
isTruthy (TFun _ _) = False -- functions cause type error
isTruthy (TDictionary _) = False -- dictionaries cause type error

typer :: Term -> Either TypeError TypeSignature
typer term = para go term M.empty
  where
    -- Each case returns a TypeReader (TypeEnv -> Either TypeError TypeSignature)
    go :: TermF (Term, TypeReader) -> TypeReader

    -- Literals don't need environment
    go (LiteralF _) = \_ -> Right TInt
    go (BoolLitF _) = \_ -> Right TBool
    go (StringLiteralF _) = \_ -> Right TString
    -- Variables look up in environment, fallback to annotation
    go (OnlyStrF (name, annotatedType)) =
      Right . fromMaybe annotatedType . M.lookup name
    go (VarF (_, innerReader)) = innerReader
    -- Let bindings: just type the RHS
    go (LetF _ (_, rhsReader)) = rhsReader
    -- Sequences: extend environment if t1 is a Let
    go (SeqF (t1, _) (_, t2Reader)) = \env -> do
      let env' = case t1 of
            Let (OnlyStr (name, _)) rhs ->
              case typer rhs of
                Right rhsType -> M.insert name rhsType env
                Left _ -> env
            _ -> env
      t2Reader env'
    go SkipF = \_ -> Right TUnit
    -- Binary operations
    go (BinaryOpsF op (_, t1Reader) (_, t2Reader)) = \env -> do
      t1Type <- t1Reader env
      t2Type <- t2Reader env
      checkBinaryOp op t1Type t2Type

    -- Unary operations
    go (UnaryOpsF op (_, tReader)) = \env -> do
      tType <- tReader env
      checkUnaryOp op tType

    -- Conditionals
    go (IfF (_, condReader) (_, thenReader) (_, elseReader)) = \env -> do
      condType <- condReader env
      thenType <- thenReader env
      elseType <- elseReader env
      if isTruthy condType
        then Right $ combine thenType elseType
        else Left $ ConditionNotBool condType

    -- Loops
    go (WhileF (_, condReader) (_, bodyReader)) = \env -> do
      condType <- condReader env
      bodyType <- bodyReader env
      if isTruthy condType
        then Right $ combine TUnit bodyType
        else Left $ ConditionNotBool condType
    go (ForF (name, varType) (_, startReader) (_, endReader) (_, bodyReader)) = \env -> do
      startType <- startReader env
      endType <- endReader env
      if startType == TInt && endType == TInt
        then do
          let env' = M.insert name varType env
          bodyType <- bodyReader env'
          Right $ combine TUnit bodyType
        else Left $ ForRangeNotInt startType endType

    -- I/O
    go (WriteF (_, tReader)) = tReader
    go (ReadF _) = \_ -> Right TInt
    -- Tuples
    go (TupleTermF termReaders) = \env -> do
      termTypes <- traverse (\(_, reader) -> reader env) termReaders
      Right $ TTuple termTypes

    -- Dictionaries
    go NewDictionaryF = \_ -> Right $ TDictionary TUnknown
    -- Collection indexing
    go (BracketF (_, collReader) (idx, idxReader)) = \env -> do
      collType <- collReader env
      idxType <- idxReader env
      case (collType, idx) of
        -- Literal index on tuple: return exact type if in bounds
        (TTuple ts, Literal n)
          | n >= 0 && fromInteger n < length ts ->
              Right $ ts !! fromInteger n
        (TTuple ts, _)
          | idxType == TInt -> Right $ TSum ts
          | otherwise -> Left $ InvalidIndexType collType idxType
        (TDictionary elemType, _)
          | idxType == TInt -> Right elemType
          | otherwise -> Left $ InvalidIndexType collType idxType
        _ -> Left $ NotIndexable collType
    go (RetrieveF (_, collReader) (key, keyReader)) = \env -> do
      collType <- collReader env
      keyType <- keyReader env
      case (collType, key) of
        -- Literal key on tuple: return exact type if in bounds
        (TTuple ts, Literal n)
          | n >= 0 && fromInteger n < length ts ->
              Right $ ts !! fromInteger n
        (TTuple ts, _)
          | keyType == TInt -> Right $ TSum ts
          | otherwise -> Left $ InvalidIndexType collType keyType
        (TDictionary elemType, _)
          | keyType == TInt -> Right elemType
          | otherwise -> Left $ InvalidIndexType collType keyType
        _ -> Left $ NotIndexable collType

    -- Merge refines dictionary types
    go (MergeF (_, collReader) _ (_, valReader)) = \env -> do
      collType <- collReader env
      valType <- valReader env
      Right $ case collType of
        TDictionary TUnknown -> TDictionary valType
        TDictionary existingType
          | existingType == valType -> collType
          | otherwise -> TDictionary (combine existingType valType)
        _ -> collType

    -- Functions: extend environment with parameters
    go (FunF params (_, bodyReader)) = \env -> do
      let paramTypes = map snd params
          paramBindings = M.fromList params
          env' = M.union paramBindings env
      bodyType <- bodyReader env'
      Right $ TFun paramTypes bodyType

    -- Function application
    go (ApplyFunF (_, funReader) argReaders) = \env -> do
      funType <- funReader env
      argTypes <- traverse (\(_, reader) -> reader env) argReaders
      case funType of
        TFun paramTypes retType
          | length paramTypes /= length argTypes ->
              Left $ ArityMismatch (length paramTypes) (length argTypes)
          | otherwise -> do
              -- Check each argument type
              let checkArg idx (expected, actual)
                    | expected == actual = Right ()
                    | otherwise = Left $ ArgumentTypeMismatch idx expected actual
              traverse_ (uncurry checkArg) (zip [0 ..] (zip paramTypes argTypes))
              Right retType
        _ -> Left $ NotAFunction funType

    -- Exception handling
    go (TryF (_, tryReader) _ (_, catchReader)) = \env -> do
      tryType <- tryReader env
      catchType <- catchReader env
      Right $ combine tryType catchType

    -- Increment/Decrement
    go (PreIncrementF _) = \_ -> Right TInt
    go (PreDecrementF _) = \_ -> Right TInt
    go (PostIncrementF _) = \_ -> Right TInt
    go (PostDecrementF _) = \_ -> Right TInt
    go BreakSignalF = \_ -> Right TUnit
    go ContinueSignalF = \_ -> Right TUnit
