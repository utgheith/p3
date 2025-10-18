module MachineState
  ( MachineState (..),
    emptyMachine,
    machineOps,
  )
where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as M
import Small (MachineOps (..))
import Value (Value (..))

data MachineState = MachineState
  { machineMemory :: M.Map String Value,
    machineInputs :: [Value],
    machineOutputs :: [Value]
  }
  deriving (Eq, Show)

emptyMachine :: MachineState
emptyMachine = MachineState M.empty [] []

type Exec a = StateT MachineState (Either String) a

machineOps :: MachineOps MachineState
machineOps =
  MachineOps
    { machineGetVar = getVar,
      machineSetVar = setVar,
      machineInput = inputVal,
      machineOutput = outputVal,
      machineAdd = numeric "addition" (+),
      machineSub = numeric "subtraction" (-),
      machineMul = numeric "multiplication" (*),
      machineDiv = divOp,
      machineMod = modOp,
      machineLt = numericCmp "<" (<),
      machineGt = numericCmp ">" (>),
      machineLte = numericCmp "<=" (<=),
      machineGte = numericCmp ">=" (>=),
      machineEq = equality,
      machineNeq = inequality,
      machineAnd = boolBinary "&&" (&&),
      machineOr = boolBinary "||" (||),
      machineNot = boolUnary "!"
    }
  where
    throw :: String -> Exec a
    throw msg = lift (Left msg)

    getVar :: String -> Exec Value
    getVar name = do
      MachineState mem _ _ <- S.get
      case M.lookup name mem of
        Just v -> pure v
        Nothing -> throw $ "get: " ++ name ++ " not found"

    setVar :: String -> Value -> Exec Value
    setVar name val = do
      MachineState mem inp out <- S.get
      let mem' = M.insert name val mem
      S.put (MachineState mem' inp out)
      pure val

    inputVal :: Exec Value
    inputVal = do
      MachineState mem inp out <- S.get
      case inp of
        (x : xs) -> do
          S.put (MachineState mem xs out)
          pure x
        [] -> throw "Input stream is empty"

    outputVal :: Value -> Exec Value
    outputVal val = do
      MachineState mem inp out <- S.get
      let out' = out ++ [val]
      S.put (MachineState mem inp out')
      pure val

    numeric :: String -> (Integer -> Integer -> Integer) -> Value -> Value -> Exec Value
    numeric _label op (IntVal v1) (IntVal v2) = pure (IntVal (op v1 v2))
    numeric label _ _ _ = throw $ "Type error in " ++ label

    divOp :: Value -> Value -> Exec Value
    divOp (IntVal v1) (IntVal v2)
      | v2 == 0 = throw "Cannot divide by 0"
      | otherwise = pure (IntVal (v1 `div` v2))
    divOp _ _ = throw "Type error in division"

    modOp :: Value -> Value -> Exec Value
    modOp (IntVal v1) (IntVal v2)
      | v2 == 0 = throw "Cannot mod by 0"
      | otherwise = pure (IntVal (v1 `mod` v2))
    modOp _ _ = throw "Type error in modulus"

    numericCmp :: String -> (Integer -> Integer -> Bool) -> Value -> Value -> Exec Value
    numericCmp _label op (IntVal v1) (IntVal v2) = pure (BoolVal (op v1 v2))
    numericCmp label _ _ _ = throw $ "Type error in " ++ label

    equality :: Value -> Value -> Exec Value
    equality (IntVal v1) (IntVal v2) = pure (BoolVal (v1 == v2))
    equality (BoolVal v1) (BoolVal v2) = pure (BoolVal (v1 == v2))
    equality (StringVal v1) (StringVal v2) = pure (BoolVal (v1 == v2))
    equality v1 v2 =
      throw $ "Type error in ==: cannot compare " ++ show v1 ++ " and " ++ show v2

    inequality :: Value -> Value -> Exec Value
    inequality (IntVal v1) (IntVal v2) = pure (BoolVal (v1 /= v2))
    inequality (BoolVal v1) (BoolVal v2) = pure (BoolVal (v1 /= v2))
    inequality (StringVal v1) (StringVal v2) = pure (BoolVal (v1 /= v2))
    inequality v1 v2 =
      throw $ "Type error in !=: cannot compare " ++ show v1 ++ " and " ++ show v2

    boolBinary :: String -> (Bool -> Bool -> Bool) -> Value -> Value -> Exec Value
    boolBinary _label op (BoolVal v1) (BoolVal v2) = pure (BoolVal (op v1 v2))
    boolBinary label _ _ _ = throw $ "Type error in " ++ label

    boolUnary :: String -> Value -> Exec Value
    boolUnary _ (BoolVal v) = pure (BoolVal (not v))
    boolUnary label _ = throw $ "Type error in " ++ label
