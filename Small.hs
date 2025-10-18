{-# LANGUAGE RankNTypes #-}

module Small
  ( Step (..),
    MachineOps (..),
    stepOnce,
    reduceFully,
  )
where

import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Term (BinaryOp (..), Term (..), TermF (..), paraTermM)
import Value (Value (..))

data Step a
  = Done a
  | Continue Term
  deriving (Eq, Show)

type Exec state = StateT state (Either String)

data MachineOps state = MachineOps
  { machineGetVar :: String -> Exec state Value,
    machineSetVar :: String -> Value -> Exec state Value,
    machineInput :: Exec state Value,
    machineOutput :: Value -> Exec state Value,
    machineAdd :: Value -> Value -> Exec state Value,
    machineSub :: Value -> Value -> Exec state Value,
    machineMul :: Value -> Value -> Exec state Value,
    machineDiv :: Value -> Value -> Exec state Value,
    machineMod :: Value -> Value -> Exec state Value,
    machineLt :: Value -> Value -> Exec state Value,
    machineGt :: Value -> Value -> Exec state Value,
    machineLte :: Value -> Value -> Exec state Value,
    machineGte :: Value -> Value -> Exec state Value,
    machineEq :: Value -> Value -> Exec state Value,
    machineNeq :: Value -> Value -> Exec state Value,
    machineAnd :: Value -> Value -> Exec state Value,
    machineOr :: Value -> Value -> Exec state Value,
    machineNot :: Value -> Exec state Value
  }

stepOnce ::
  MachineOps state ->
  Term ->
  state ->
  (Either String (Step Value), state)
stepOnce ops term initialState =
  case runStateT (paraTermM (smallStepAlg ops) term) initialState of
    Left err -> (Left err, initialState)
    Right (result, newState) -> (Right result, newState)

reduceFully ::
  MachineOps state ->
  Term ->
  state ->
  (Either String Value, state)
reduceFully ops = go
  where
    go currentTerm currentState =
      case runStateT (paraTermM (smallStepAlg ops) currentTerm) currentState of
        Left err -> (Left err, currentState)
        Right (Done value, newState) -> (Right value, newState)
        Right (Continue nextTerm, newState) -> go nextTerm newState

smallStepAlg ::
  MachineOps state ->
  TermF (Term, Exec state (Step Value)) ->
  Exec state (Step Value)
smallStepAlg ops termF =
  case termF of
    LiteralF n -> pure (Done (IntVal n))
    StringLiteralF s -> pure (Done (StringVal s))
    BoolLitF b -> pure (Done (BoolVal b))
    VarF name -> doneFrom (machineGetVar ops name)
    LetF name (_, evaluateBound) -> do
      boundResult <- evaluateBound
      case boundResult of
        Continue next -> pure (Continue (Let name next))
        Done value -> doneFrom (machineSetVar ops name value)
    SeqF (_, evaluateFirst) (secondTerm, _) -> do
      firstResult <- evaluateFirst
      case firstResult of
        Continue next -> pure (Continue (Seq next secondTerm))
        Done _ -> pure (Continue secondTerm)
    IfF (_, evaluateCond) (thenTerm, _) (elseTerm, _) -> do
      condResult <- evaluateCond
      case condResult of
        Continue nextCond -> pure (Continue (If nextCond thenTerm elseTerm))
        Done value ->
          case truthy value of
            Right True -> pure (Continue thenTerm)
            Right False -> pure (Continue elseTerm)
            Left msg -> lift (Left msg)
    WhileF (condTerm, _) (bodyTerm, _) ->
      pure (Continue (If condTerm (Seq bodyTerm (While condTerm bodyTerm)) Skip))
    ReadF name -> do
      inputValue <- machineInput ops
      doneFrom (machineSetVar ops name inputValue)
    WriteF (_, evaluateTerm) -> do
      result <- evaluateTerm
      case result of
        Continue next -> pure (Continue (Write next))
        Done value -> doneFrom (machineOutput ops value)
    SkipF -> pure (Done (IntVal 0))
    BinaryOpsF op left right ->
      binaryStep (BinaryOps op) left right (numericOp op)
    LtF left right ->
      binaryStep Lt left right (machineLt ops)
    GtF left right ->
      binaryStep Gt left right (machineGt ops)
    LteF left right ->
      binaryStep Lte left right (machineLte ops)
    GteF left right ->
      binaryStep Gte left right (machineGte ops)
    EqF left right ->
      binaryStep Eq left right (machineEq ops)
    NeqF left right ->
      binaryStep Neq left right (machineNeq ops)
    AndF left right ->
      binaryStep And left right (machineAnd ops)
    OrF left right ->
      binaryStep Or left right (machineOr ops)
    NotF (_, evaluateTerm) -> do
      result <- evaluateTerm
      case result of
        Continue next -> pure (Continue (Not next))
        Done value -> doneFrom (machineNot ops value)
  where
    numericOp opType =
      case opType of
        Add -> machineAdd ops
        Sub -> machineSub ops
        Mul -> machineMul ops
        Div -> machineDiv ops
        Mod -> machineMod ops

    doneFrom :: Exec state Value -> Exec state (Step Value)
    doneFrom action = Done <$> action

    binaryStep ::
      (Term -> Term -> Term) ->
      (Term, Exec state (Step Value)) ->
      (Term, Exec state (Step Value)) ->
      (Value -> Value -> Exec state Value) ->
      Exec state (Step Value)
    binaryStep rebuild (_originalLeft, evaluateLeft) (rightTerm, evaluateRight) apply = do
      leftResult <- evaluateLeft
      case leftResult of
        Continue nextLeft -> pure (Continue (rebuild nextLeft rightTerm))
        Done leftValue -> do
          rightResult <- evaluateRight
          case rightResult of
            Continue nextRight ->
              pure (Continue (rebuild (quoteValue leftValue) nextRight))
            Done rightValue -> doneFrom (apply leftValue rightValue)

quoteValue :: Value -> Term
quoteValue (IntVal n) = Literal n
quoteValue (BoolVal b) = BoolLit b
quoteValue (StringVal s) = StringLiteral s

truthy :: Value -> Either String Bool
truthy (BoolVal b) = Right b
truthy (IntVal n) = Right (n /= 0)
truthy (StringVal s) = Right (not (null s))
