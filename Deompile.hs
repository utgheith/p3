module Deompile(decompile) where

import Term(Term, TermF(..))
import Data.Functor.Foldable (cata)
import Sprintf (sprintf)

decompile :: Term -> String
decompile = cata go where
    go :: TermF String -> String
    go (IfF cond thenBranch elseBranch) =
        sprintf "If (%s) (%s) (%s)" [cond, thenBranch, elseBranch]
    go (TryF tryBlock errKindOrAny catchBlock) =
        sprintf "Try (%s) (%s) (%s)" [tryBlock, show errKindOrAny, catchBlock]
    go (LiteralF n) =
        show n
    go (StringLiteralF s) =
        show s
    go (ReadF s) =
        sprintf "Read %s" [s]
    go (SeqF t1 t2) =
        sprintf "(%s) ; (%s)" [t1, t2]
    go SkipF =
        "Skip"
    go (BinaryOpsF op t1 t2) =
        sprintf "(%s %s %s)" [t1, show op, t2]
    go (UnaryOpsF op t) =
        sprintf "%s(%s)" [show op, t]
    go (VarF _) =
        sprintf "Var %s" (error "decompile: Ref decompilation not implemented")
    go (WhileF cond body) =
        sprintf "While (%s) (%s)" [cond, body]
    go (WriteF t) =
        sprintf "Write (%s)" [t]
    go (BoolLitF b) =
        show b
    go (TupleTermF terms) =
        sprintf "TupleTerm [%s]" [concatMap (\t -> t ++ ", ") terms]
    go NewDictionaryF =
        "NewDictionary"
    go (RetrieveF dict index) =
        sprintf "Retrieve (%s) (%s)" [dict, index]
    go (MergeF current index value) =
        sprintf "Merge (%s) (%s) (%s)" [current, index, value]
    go (FunF args body) =
        sprintf "Fun [%s] (%s)" [concatMap (\a -> a ++ ", ") args, body]
    go (ApplyFunF fun args) =
        sprintf "ApplyFun (%s) [%s]" [fun, concatMap (\a -> a ++ ", ") args]
    go (PreIncrementF varName) =
        sprintf "PreIncrement %s" [varName]
    go (PreDecrementF varName) =
        sprintf "PreDecrement %s" [varName]
    go (PostIncrementF varName) =
        sprintf "PostIncrement %s" [varName]
    go (PostDecrementF varName) =
        sprintf "PostDecrement %s" [varName]
    go BreakSignalF =
        "BreakSignal"
    go ContinueSignalF =
        "ContinueSignal"
    go (LetF _ t) =
        sprintf "Let (%s) (%s)" [error "decompile: Ref decompilation not implemented", t]