{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module ConSpec (spec) where 

import qualified Control.Monad.State as S
import qualified Data.Map as M
import Scope (Scope (..), emptyScope, getAllBindings, insertScope, lookupScope)
import Small
import Term
import Test.Hspec
import Value (Value (..))
import qualified System.Random as R

-- Same as mock machine, but with true randomness
data MulticoreMachine = MulticoreMachine {getMem :: Scope, getInput :: [Value], getOutput :: [Value], getRng :: R.StdGen} deriving (Show, Eq)

decide :: R.RandomGen g => g -> a -> a -> (a, g)
decide rng a1 a2 = 
  let (num, rng') = R.uniformR (0 :: Int, 1 :: Int) rng in
    (if num == 0 then a1 else a2, rng')

instance Machine MulticoreMachine where
  type V MulticoreMachine = Value

  getVar x = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just v -> return $ Happy v
      Nothing -> return $ Sad "variable not found"

  setVar x v = do
    m <- S.get
    S.put (m {getMem = insertScope x v (getMem m)})
    return $ Happy v

  getScope m = getAllBindings (getMem m)

  pushScope vars = do
    m <- S.get
    S.put (m {getMem = Scope (M.fromList vars) (Just (getMem m))})
    return $ Happy (IntVal 0)

  popScope = do
    m <- S.get
    case getMem m of
      Scope _ (Just parent) -> S.put (m {getMem = parent})
      Scope _ Nothing -> S.put (m {getMem = emptyScope}) -- Reset to empty scope.
    return $ Happy (IntVal 0)

  inputVal = do
    m <- S.get
    case getInput m of
      (i : is) -> do
        S.put (m {getInput = is})
        return $ Happy i
      [] -> return $ Sad "end of input"

  outputVal v = do
    m <- S.get
    S.put (m {getOutput = getOutput m ++ [v]})
    return $ Happy v

  subVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 - v2))
  subVal _ _ = return $ Sad "Type error in subtraction"

  addVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 + v2))
  addVal _ _ = return $ Sad "Type error in addition"

  mulVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 * v2))
  mulVal _ _ = return $ Sad "Type error in multiplication"

  divVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad "Cannot divide by 0"
      else return $ Happy (IntVal (v1 `div` v2)) -- I don't want the actual interpreter to crash
  divVal _ _ = return $ Sad "Type error in division"

  modVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad "Cannot mod by 0"
      else return $ Happy (IntVal (v1 `mod` v2)) -- I don't want the actual interpreter to crash
  modVal _ _ = return $ Sad "Type error in modulus"

  negVal (IntVal v) =
    return $ Happy (IntVal (-v))
  negVal _ = return $ Sad "Type error in neg"

  ltVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 < v2))
  ltVal _ _ = return $ Sad "Type error in <"

  gtVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 > v2))
  gtVal _ _ = return $ Sad "Type error in >"

  lteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 <= v2))
  lteVal _ _ = return $ Sad "Type error in <="

  gteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 >= v2))
  gteVal _ _ = return $ Sad "Type error in >="

  eqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal _ _ = return $ Sad "Type error in =="

  neqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal _ _ = return $ Sad "Type error in !="

  andVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 && v2))
  andVal _ _ = return $ Sad "Type error in &&"

  orVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 || v2))
  orVal _ _ = return $ Sad "Type error in ||"

  notVal (BoolVal v) = return $ Happy (BoolVal (not v))
  notVal _ = return $ Sad "Type error in !"

  getTupleValue (Tuple (x : xs)) (IntVal pos) = if pos == 0 then return (Happy x) else getTupleValue (Tuple xs) (IntVal (pos - 1))
  getTupleValue _ _ = return $ Sad "Tuple Lookup Bad Input"

  setTupleValue n t v = do
    m <- S.get
    case lookupScope n (getMem m) of
      Just oldVal -> case oldVal of
        Tuple _ ->
          let newVal = updateTuple oldVal t v
           in case newVal of
                Just newVal' -> do
                  S.put (m {getMem = insertScope n newVal' (getMem m)})
                  return $ Happy v
                Nothing -> return $ Sad "Something went wrong while trying to update Tuple value"
        _ -> return $ Sad "Attempting to Index but didn't find Tuple"
      Nothing -> return $ Sad "Attempting to Set Tuple That Doesn't Exist"
    where
      updateTuple :: Value -> Value -> Value -> Maybe Value
      updateTuple (Tuple (x : xs)) (Tuple (y : ys)) val = case y of
        IntVal index ->
          if index == 0
            then
              let returnVal = updateTuple x (Tuple ys) val
               in case returnVal of
                    Just a -> Just $ Tuple (a : xs)
                    Nothing -> Nothing
            else
              let returnVal = updateTuple (Tuple xs) (Tuple (IntVal (index - 1) : ys)) val
               in case returnVal of
                    Just (Tuple a) -> Just $ Tuple (x : a)
                    Nothing -> Nothing
                    _ -> error "Unable to rebuild tuple"
        _ -> Nothing
      updateTuple _ (Tuple []) val = Just val
      updateTuple _ _ _ = Nothing

  selectValue (BoolVal True) c _ = c
  selectValue (BoolVal False) _ t = t
  selectValue (IntVal n) c t = if n /= 0 then c else t
  selectValue (StringVal s) c t = if not (null s) then c else t
  selectValue (Tuple l) c t = if not (null l) then c else t
  selectValue (ClosureVal {}) _ _ = return $ Sad "Type error in select"

  selectRandom m e1 e2 = 
    let (e', rng') = decide (getRng m) e1 e2 in do
      v <- e'
      m' <- S.get
      S.put m'{getRng=rng'}
      return v

spec :: Spec
spec = do
    describe "concurrent reduce" $ do 
        let initialMachine seed = MulticoreMachine{getMem= emptyScope, getInput = [], getOutput = [], getRng= R.mkStdGen seed}
        it "Can pick first value" $ do 
            let term = ConcurSeq (Literal 1) (Literal 2)
            fst (reduceFully term (initialMachine 42)) `shouldBe` (Right (IntVal 1))
        it "Can pick second value" $ do 
            let term = ConcurSeq (Literal 1) (Literal 2)
            fst (reduceFully term (initialMachine 43)) `shouldBe` (Right (IntVal 2))
        it "We have data races" $ do
            let addop = BinaryOps Add (Var "x") (Literal 1)
            let assign = Let "x" (If (Literal 1) (addop) (addop))
            let initx = Let "x" (Literal 0)
            let term = Seq (initx) (ConcurSeq assign assign)
            let results = [ fst (reduceFully term (initialMachine seed)) | seed <- [1..100]]
            -- we should see both 2 and 3 as possible results
            results `shouldSatisfy` (\res -> (Right (IntVal 1)) `elem` res && (Right (IntVal 2)) `elem` res)
