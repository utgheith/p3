{-# LANGUAGE DataKinds #-}

module Typer (typer, Types (..)) where

import Data.Functor.Foldable (para)
import Term (BinaryOp (..), Term, TermF (..), UnaryOp (..))

data Types = TBool | TFun Types | TInt | TString | TSum Types Types | TUnit | TUnknown deriving (Eq, Show)

-- Union type vs. Sum type
combine :: Types -> Types -> Types
combine TUnknown _ = TUnknown
combine _ TUnknown = TUnknown
combine t1 t2 | t1 == t2 = t1 --  this makes a union type, Python semantics
combine t1 t2 = TSum t1 t2

(-->) :: Bool -> Types -> Types
(-->) True t = t
(-->) False _ = TUnknown

typer :: Term -> Types
typer = para go
  where
    go (LiteralF _) = TInt
    go (BoolLitF _) = TBool
    go (StringLiteralF _) = TString
    go (VarF _) = TUnit
    go (LetF _ (_, tType)) = tType
    go (BinaryOpsF op (_, t1Type) (_, t2Type)) =
      case op of
        -- TODO: how about TString + ...? TString * ...?
        Add -> (t1Type == TInt && t2Type == TInt) --> TInt
        Sub -> (t1Type == TInt && t2Type == TInt) --> TInt
        Mul -> (t1Type == TInt && t2Type == TInt) --> TInt
        Div -> (t1Type == TInt && t2Type == TInt) --> TInt
        Mod -> (t1Type == TInt && t2Type == TInt) --> TInt
        Lt -> (t1Type == TInt && t2Type == TInt) --> TBool
        Gt -> (t1Type == TInt && t2Type == TInt) --> TBool
        Lte -> (t1Type == TInt && t2Type == TInt) --> TBool
        Gte -> (t1Type == TInt && t2Type == TInt) --> TBool
        Eq -> (t1Type == t2Type) --> TBool
        Neq -> (t1Type == t2Type) --> TBool
        And -> (t1Type == TBool && t2Type == TBool) --> TBool
        Or -> (t1Type == TBool && t2Type == TBool) --> TBool
        Pow -> (t1Type == TInt && t2Type == TInt) --> TInt
        Xor -> (t1Type == TInt && t2Type == TInt) --> TInt
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
    go (WhileF (_, condType) (_, bodyType)) =
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
    go (FunF _ (_, bodyType)) =
      TFun bodyType
    go (ApplyFunF (_, TFun retType) _) = retType
    go (ApplyFunF _ _) = TUnknown
    -- TODO: complete other cases
    go _ =
      TUnknown
