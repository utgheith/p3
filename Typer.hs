{-# LANGUAGE DataKinds #-}

module Typer (typer) where

import Data.Functor.Foldable (para)
import System.IO (Handle, hPutStrLn)
import Term (BinaryOp (..), Term, TermF (..), UnaryOp (..))
import TypeSignature (TypeSignature (..))

-- Union type vs. Sum type
combine :: TypeSignature -> TypeSignature -> TypeSignature
combine TUnknown _ = TUnknown
combine _ TUnknown = TUnknown
combine t1 t2 | t1 == t2 = t1 -- this makes a union type, Python semantics
combine t1 t2 = TSum [t1, t2]

(-->) :: Bool -> TypeSignature -> TypeSignature
(-->) True t = t
(-->) False _ = TUnknown

-- TODO: turn this into a real type checker with context, ...
typer :: Handle -> Term -> IO (Either String TypeSignature)
typer debugFile term = do
  _ <- hPutStrLn debugFile "Starting typer"
  let out = para go term
  return $ Right out
  where
    go (LiteralF _) = TInt
    go (BoolLitF _) = TBool
    go (StringLiteralF _) = TString
    go (VarF _) = TUnit
    go (LetF _ (_, tType)) = tType
    go (BinaryOpsF op (_, t1Type) (_, t2Type)) =
      let both t = t1Type == t && t2Type == t
       in case op of
            -- TODO: how about TString + ...? TString * ...?
            Add -> both TInt --> TInt
            Sub -> both TInt --> TInt
            Mul -> both TInt --> TInt
            Div -> both TInt --> TInt
            Mod -> both TInt --> TInt
            Lt -> both TInt --> TBool
            Gt -> both TInt --> TBool
            Lte -> both TInt --> TBool
            Gte -> both TInt --> TBool
            Eq -> both t1Type --> TBool
            Neq -> both t1Type --> TBool
            And -> both TBool --> TBool
            Or -> both TBool --> TBool
            Pow -> both TInt --> TInt
            Xor -> both TBool --> TBool
    go (UnaryOpsF op (_, tType)) =
      case op of
        Not -> (tType == TBool) --> TBool
        Neg -> (tType == TInt) --> TInt
        BitNot -> (tType == TInt) --> TInt
    go (SeqF _ (_, t2Type)) = t2Type
    go (IfF (_, condType) (_, thenType) (_, elseType)) =
      (condType == TBool) --> combine thenType elseType
    go (WriteF (_, tType)) =
      tType
    go (WhileF (_, condType) (_, bodyType) _ _) =
      (condType == TBool) --> combine TUnit bodyType
    -- TODO: lookup in typing context
    go (OnlyStrF _) =
      TUnknown
    -- TODO: array types?
    go (BracketF _ _) =
      TUnknown
    -- TODO: do those need to be AST nodes? Why a string?
    go (PreIncrementF _) =
      TInt
    go (PreDecrementF _) =
      TInt
    go (PostIncrementF _) =
      TInt
    go (PostDecrementF _) =
      TInt
    -- TODO: exception types
    go (TryF (_, tryType) _ (_, catchType)) =
      combine tryType catchType
    -- TODO: why int?
    go (ReadF _) =
      TInt
    -- TODO: handle arg count and types
    go (FunF args (_, bodyType)) =
      TFun (snd <$> args) bodyType
    go (ApplyFunF (_, TFun _ retType) _) = retType
    go (ApplyFunF _ _) = TUnknown
    -- TODO: complete other cases
    go _ =
      TUnknown
