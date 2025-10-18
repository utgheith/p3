{-# LANGUAGE DeriveTraversable #-}

module Term
  ( Term (..),
    BinaryOp (..),
    TermF (..),
    projectTerm,
    embedTerm,
    paraTermM,
  )
where

data BinaryOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Show)

data Term
  = If Term Term Term
  | Let String Term
  | Literal Integer
  | StringLiteral String
  | Read String
  | Seq Term Term
  | Skip
  | BinaryOps BinaryOp Term Term
  | Var String
  | While Term Term
  | Write Term
  | BoolLit Bool
  | Lt Term Term
  | Gt Term Term
  | Lte Term Term
  | Gte Term Term
  | Eq Term Term
  | Neq Term Term
  | And Term Term
  | Or Term Term
  | Not Term
  deriving (Eq, Show)

data TermF a
  = IfF a a a
  | LetF String a
  | LiteralF Integer
  | StringLiteralF String
  | ReadF String
  | SeqF a a
  | SkipF
  | BinaryOpsF BinaryOp a a
  | VarF String
  | WhileF a a
  | WriteF a
  | BoolLitF Bool
  | LtF a a
  | GtF a a
  | LteF a a
  | GteF a a
  | EqF a a
  | NeqF a a
  | AndF a a
  | OrF a a
  | NotF a
  deriving (Eq, Show, Functor, Foldable, Traversable)

projectTerm :: Term -> TermF Term
projectTerm (If c t e) = IfF c t e
projectTerm (Let name body) = LetF name body
projectTerm (Literal n) = LiteralF n
projectTerm (StringLiteral s) = StringLiteralF s
projectTerm (Read name) = ReadF name
projectTerm (Seq t1 t2) = SeqF t1 t2
projectTerm Skip = SkipF
projectTerm (BinaryOps op t1 t2) = BinaryOpsF op t1 t2
projectTerm (Var name) = VarF name
projectTerm (While cond body) = WhileF cond body
projectTerm (Write t) = WriteF t
projectTerm (BoolLit b) = BoolLitF b
projectTerm (Lt t1 t2) = LtF t1 t2
projectTerm (Gt t1 t2) = GtF t1 t2
projectTerm (Lte t1 t2) = LteF t1 t2
projectTerm (Gte t1 t2) = GteF t1 t2
projectTerm (Eq t1 t2) = EqF t1 t2
projectTerm (Neq t1 t2) = NeqF t1 t2
projectTerm (And t1 t2) = AndF t1 t2
projectTerm (Or t1 t2) = OrF t1 t2
projectTerm (Not t) = NotF t

embedTerm :: TermF Term -> Term
embedTerm (IfF c t e) = If c t e
embedTerm (LetF name body) = Let name body
embedTerm (LiteralF n) = Literal n
embedTerm (StringLiteralF s) = StringLiteral s
embedTerm (ReadF name) = Read name
embedTerm (SeqF t1 t2) = Seq t1 t2
embedTerm SkipF = Skip
embedTerm (BinaryOpsF op t1 t2) = BinaryOps op t1 t2
embedTerm (VarF name) = Var name
embedTerm (WhileF cond body) = While cond body
embedTerm (WriteF t) = Write t
embedTerm (BoolLitF b) = BoolLit b
embedTerm (LtF t1 t2) = Lt t1 t2
embedTerm (GtF t1 t2) = Gt t1 t2
embedTerm (LteF t1 t2) = Lte t1 t2
embedTerm (GteF t1 t2) = Gte t1 t2
embedTerm (EqF t1 t2) = Eq t1 t2
embedTerm (NeqF t1 t2) = Neq t1 t2
embedTerm (AndF t1 t2) = And t1 t2
embedTerm (OrF t1 t2) = Or t1 t2
embedTerm (NotF t) = Not t

paraTermM :: (Monad m) => (TermF (Term, m r) -> m r) -> Term -> m r
paraTermM alg = go
  where
    go term = alg (fmap (\sub -> (sub, go sub)) (projectTerm term))
