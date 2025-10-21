{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Small (reduceFully, Machine (..), Result (..), Error, Env) where

import Control.Arrow ((>>>))
import qualified Control.Monad.State as S
import Data.Either
import qualified Data.Map as M
import Debug.Trace (trace)
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Term (..), UnaryOp (..))
import Value (Value (..), valueToInt, valueToTuple)

----- The Machine type class -----

-- The micro-ops that a machine must support
-- Allow an implementation to define its own semantics

class Machine m where
  type V m -- The value type for this machine
  -- Uses associated an associated type family for the value type
  -- This requires the TypeFamilies extension
  -- The way you read the type signature is:
  --    for any type m that is an instance of Machine, there is an associated type (V m)

  -- Get and set variables
  getVar :: String -> Env m
  setVar :: String -> V m -> Env m

  -- Lexical scoping
  getScope :: m -> [(String, Value)] -- Variable bindings only.
  pushScope :: [(String, Value)] -> Env m
  popScope :: Env m

  -- I/O
  inputVal :: Env m
  outputVal :: V m -> Env m

  -- Arithmetic and control
  addVal :: V m -> V m -> Env m
  subVal :: V m -> V m -> Env m
  mulVal :: V m -> V m -> Env m
  divVal :: V m -> V m -> Env m
  modVal :: V m -> V m -> Env m
  powVal :: V m -> V m -> Env m
  negVal :: V m -> Env m

  -- Comparison operations (operate on integers, return booleans)
  ltVal :: V m -> V m -> Env m
  gtVal :: V m -> V m -> Env m
  lteVal :: V m -> V m -> Env m
  gteVal :: V m -> V m -> Env m
  eqVal :: V m -> V m -> Env m
  neqVal :: V m -> V m -> Env m

  -- Logical operations (operate on booleans)
  andVal :: V m -> V m -> Env m
  orVal :: V m -> V m -> Env m
  xorVal :: V m -> V m -> Env m
  notVal :: V m -> Env m
  bitNotVal :: V m -> Env m

  -- Increment/Decrement operations (modify variables)
  preIncrementVal :: String -> Env m -- ++x: increment then return new value
  preDecrementVal :: String -> Env m -- --x: decrement then return new value
  postIncrementVal :: String -> Env m -- x++: return old value then increment
  postDecrementVal :: String -> Env m -- x--: return old value then decrement

  -- Access/Manage Bracket Values
  getBracketValue :: V m -> V m -> Env m
  setBracketValue :: String -> V m -> V m -> Env m

  -- Control flow - selectValue uses boolean semantics
  selectValue :: V m -> Env m -> Env m -> Env m

----- The Result type -----

type Error = (ErrorKind, String)

data Result a
  = Happy a -- produced an answer
  | Continue Term -- need to keep going
  | Sad Error -- error
  deriving (Eq, Show)

----- The Env monad -----

-- abstract semantics that glue micro-ops together

type Env m = S.State m (Result (V m))

premise :: Env m -> (Term -> Term) -> (V m -> Env m) -> Env m
premise e l r = do
  v <- e
  case v of
    Continue BreakSignal -> return $ Continue BreakSignal
    Continue ContinueSignal -> return $ Continue ContinueSignal
    Continue t' -> return $ Continue (l t')
    Happy n -> r n
    Sad _ -> return v

-- Helper for try-catch statement
errorShouldBeCaught :: ErrorKind -> ErrorKindOrAny -> Bool
errorShouldBeCaught _ Any = True
errorShouldBeCaught resultErrorKind (Specific catchableErrorKind) = resultErrorKind == catchableErrorKind

------ Small-step reduction ------

reduce_ :: (Machine m, Show m, V m ~ Value) => Term -> Env m
reduce_ (Literal n) =
  return $ Happy $ IntVal n
reduce_ (StringLiteral s) =
  return $ Happy $ StringVal s
reduce_ (Var x) =
  getVar x
reduce_ (Let x t) = do
  premise
    (reduce t)
    (Let x)
    (setVar x)
reduce_ (Seq t1 t2) = do
  res1 <- reduce t1 -- run t1 normally
  case res1 of
    Continue BreakSignal -> return $ Continue BreakSignal
    Continue ContinueSignal -> return $ Continue ContinueSignal
    Continue t' -> return $ Continue (Seq t' t2)
    Happy _ -> reduce t2 -- normal: continue with t2
    Sad msg -> return $ Sad msg
reduce_ (If cond tThen tElse) = do
  premise
    (reduce cond)
    (\cond' -> If cond' tThen tElse)
    (\v -> selectValue v (return $ Continue tThen) (return $ Continue tElse))
reduce_ (Try tTry catchableErrorKindOrAny tCatch) = do
  vTry <- reduce tTry
  case vTry of
    Continue tTry' -> return $ Continue (Try tTry' catchableErrorKindOrAny tCatch)
    Happy n -> return $ Happy n
    Sad (resultErrorKind, _) | errorShouldBeCaught resultErrorKind catchableErrorKindOrAny -> return $ Continue tCatch
    Sad _ -> return vTry
reduce_ (While cond body) = do
  premise
    (reduce cond)
    (`While` body)
    ( \v -> do
        selectValue
          v
          ( do
              res <- reduce body
              case res of
                Continue BreakSignal -> do return $ Happy (IntVal 0)
                Continue ContinueSignal -> do return $ Continue (While cond body)
                Continue t -> do return $ Continue (Seq t (While cond body))
                Happy _ -> do return $ Continue (While cond body)
                Sad msg -> do return $ Sad msg
          )
          ( do
              return $ Continue Skip
          )
    )
reduce_ (Read x) =
  premise
    inputVal
    id
    (setVar x)
reduce_ (Write t) = do
  premise
    (reduce t)
    Write
    outputVal
reduce_ Skip =
  return $ Happy (IntVal 0)
reduce_ (BinaryOps op t1 t2) =
  premise
    (reduce t1)
    (\t1' -> BinaryOps op t1' t2)
    ( \v1 ->
        premise
          (reduce t2)
          (BinaryOps op (Literal $ fromRight (-1) (valueToInt v1)))
          (applyBinaryOp op v1)
    )
  where
    applyBinaryOp Add = addVal
    applyBinaryOp Sub = subVal
    applyBinaryOp Mul = mulVal
    applyBinaryOp Div = divVal
    applyBinaryOp Mod = modVal
    applyBinaryOp Pow = powVal
    applyBinaryOp Lt = ltVal
    applyBinaryOp Gt = gtVal
    applyBinaryOp Lte = lteVal
    applyBinaryOp Gte = gteVal
    applyBinaryOp Eq = eqVal
    applyBinaryOp Neq = neqVal
    applyBinaryOp And = andVal
    applyBinaryOp Or = orVal
    applyBinaryOp Xor = xorVal
reduce_ (BoolLit b) =
  return $ Happy $ BoolVal b
reduce_ (UnaryOps op t) =
  premise
    (reduce t)
    (UnaryOps op)
    (applyUnaryOp op)
  where
    applyUnaryOp Neg = negVal
    applyUnaryOp Not = notVal
    applyUnaryOp BitNot = bitNotVal
reduce_ BreakSignal =
  return $ Continue BreakSignal
reduce_ ContinueSignal =
  return $ Continue ContinueSignal
reduce_ (Fun xs t) = do
  env <- S.get
  let vars = getScope env
  return $ Happy (ClosureVal xs t vars)
reduce_ (ApplyFun tf tas) =
  premise
    (reduce tf)
    (`ApplyFun` tas)
    (reduceArgsAndApply tf tas)
reduce_ (PreIncrement x) =
  preIncrementVal x
reduce_ (PreDecrement x) =
  preDecrementVal x
reduce_ (PostIncrement x) =
  postIncrementVal x
reduce_ (PostDecrement x) =
  postDecrementVal x
reduce_ (TupleTerm elements) =
  case elements of
    (x : xs) ->
      premise
        (reduce x)
        (\term' -> TupleTerm $ term' : xs)
        ( \v1 ->
            premise
              (reduce $ TupleTerm xs)
              ( \case
                  TupleTerm xs' -> TupleTerm (x : xs')
                  _ -> error "TupleTerm recursion somehow returned a non TupleTerm continuation"
              )
              (\v2 -> return $ Happy $ Tuple $ v1 : fromRight [] (valueToTuple v2))
        )
    [] -> return $ Happy $ Tuple []
reduce_ (AccessBracket t i) =
  premise
    (reduce t)
    (`AccessBracket` i)
    (getBracketValue >>> premise (reduce i) (AccessBracket t)) -- (f >>> g) == (g . f) == (\x -> g (f x))
reduce_ (SetBracket name terms val) =
  case terms of
    TupleTerm tupleTerm ->
      premise
        (reduce $ TupleTerm tupleTerm)
        (\terms' -> SetBracket name terms' val)
        ( setBracketValue name
            >>> premise
              (reduce val)
              (SetBracket name terms)
        )
    _ -> error "SetBracket should only have tuple term as second argument"
reduce_ NewDictionary =
  return $ Happy $ Dictionary M.empty
-- Traditional for loop: for (var = init; cond; incr) body
-- Translates to: let var = init; while (cond) (body; incr)
reduce_ (ForLoop varName initExpr condExpr incrExpr body) = do
  return $
    Continue $
      Seq
        (Let varName initExpr)
        (While condExpr (Seq body incrExpr))

-- For-each loop: for each var in iterable body
-- Translates to iterating through each element of a tuple/list
reduce_ (ForEach varName iterable body) = do
  premise
    (reduce iterable)
    (\iter' -> ForEach varName iter' body)
    ( \case
        Tuple [] -> return $ Continue Skip
        Tuple (x : xs) -> do
          let restIterable = TupleTerm (map valueToTerm xs)
              currentIteration = Let varName (valueToTerm x)
              nextIterations = ForEach varName restIterable body
          return $ Continue $ Seq currentIteration (Seq body nextIterations)
        _ -> return $ Sad (Type, "for-each requires a tuple/list")
    )

-- Compound assignment: variable += expression
reduce_ (AddAssign varName expr) = do
  premise
    (reduce expr)
    (AddAssign varName)
    ( \exprVal -> do
        varVal <- getVar varName
        case varVal of
          Happy v -> do
            result <- addVal v exprVal
            case result of
              Happy newVal -> setVar varName newVal
              _ -> return result
          _ -> return varVal
    )

-- Compound assignment: variable -= expression
reduce_ (SubAssign varName expr) = do
  premise
    (reduce expr)
    (SubAssign varName)
    ( \exprVal -> do
        varVal <- getVar varName
        case varVal of
          Happy v -> do
            result <- subVal v exprVal
            case result of
              Happy newVal -> setVar varName newVal
              _ -> return result
          _ -> return varVal
    )

-- Helper function to convert Value back to Term
valueToTerm :: Value -> Term
valueToTerm (IntVal n) = Literal n
valueToTerm (BoolVal b) = BoolLit b
valueToTerm (StringVal s) = StringLiteral s
valueToTerm (Tuple vs) = TupleTerm (map valueToTerm vs)
valueToTerm v = error $ "valueToTerm: unsupported value type: " ++ show v

reduceArgsAndApply :: (Machine m, Show m, V m ~ Value) => Term -> [Term] -> Value -> Env m
reduceArgsAndApply tf args funVal =
  case args of
    [] -> applyFuncNoArg funVal
    (a : rest) ->
      premise
        (reduce a)
        (\a' -> ApplyFun tf (a' : rest))
        (applyFunArgList tf rest funVal)

applyFunArgList :: (Machine m, Show m, V m ~ Value) => Term -> [Term] -> Value -> Value -> Env m
applyFunArgList tf rest funVal argVal = do
  res1 <- applyFunArg funVal argVal
  case res1 of
    Happy v1 -> case rest of
      [] -> return (Happy v1)
      _ -> reduceArgsAndApply tf rest v1
    Continue t -> return (Continue t)
    Sad msg -> return (Sad msg)

applyFunArg :: (Machine m, Show m, V m ~ Value) => Value -> Value -> Env m
applyFunArg (ClosureVal [] _ _) _ = do
  return $ Sad (Arguments, "too many arguments: function takes 0 arguments")
applyFunArg (ClosureVal (x : xs) body caps) arg = do
  let newCaps = caps ++ [(x, arg)]
  if null xs
    then evalClosureBody body newCaps
    else return $ Happy (ClosureVal xs body newCaps)
applyFunArg _ _ = return $ Sad (Type, "attempt to call a non-function")

applyFuncNoArg :: (Machine m, Show m, V m ~ Value) => Value -> Env m
applyFuncNoArg (ClosureVal [] body caps) = evalClosureBody body caps
applyFuncNoArg (ClosureVal (_ : _) _ _) = return $ Sad (Arguments, "missing arguments: function requires parameters")
applyFuncNoArg _ = return $ Sad (Type, "attempt to call a non-function")

-- Bind captured args, evaluate body, restore machine state
evalClosureBody :: (Machine m, Show m, V m ~ Value) => Term -> [(String, Value)] -> Env m
evalClosureBody body caps = do
  m0 <- S.get
  let (_resPush, m1) = S.runState (pushScope []) m0
  case bindMany caps m1 of
    Left msg -> return $ Sad msg
    Right m2 -> do
      let (res, m3) = reduceFully body m2
      let (_resPop, m4) = S.runState popScope m3 -- Restore previous scope.
      S.put m4
      case res of
        Left msg -> return $ Sad (Arguments, msg)
        Right v -> return $ Happy v

bindMany :: (Machine m, V m ~ Value) => [(String, Value)] -> m -> Either Error m
bindMany [] m = Right m
bindMany ((k, v) : rest) m =
  case S.runState (setVar k v) m of
    (Sad msg, _m') -> Left msg
    (Continue _, _m') -> Left (Arguments, "internal: setVar requested Continue")
    (Happy _, m') -> bindMany rest m'

reduce :: (Machine m, Show m, V m ~ Value) => Term -> Env m
reduce t = do
  e <- S.get
  trace ("Simulating: " ++ show t) () `seq`
    trace ("     Machine: " ++ show e) () `seq`
      reduce_ t

reduceFully :: (Machine m, Show m, V m ~ Value) => Term -> m -> (Either String (V m), m)
reduceFully term machine =
  case S.runState (reduce term) machine of
    (Sad (_, message), m) -> (Left message, m)
    (Continue t, m) -> do
      case t of
        BreakSignal -> (Left "unhandled break signal", m)
        ContinueSignal -> (Left "unhandled continue signal", m)
        _ -> reduceFully t m
    (Happy n, m) -> (Right n, m)
