
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module SmallSpec (spec) where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import Small
import Term
import Value (Value(..))
import Test.Hspec

-- A mock machine for testing
data MockMachine = MockMachine { getMem :: M.Map String Value, getInput :: [Value], getOutput :: [Value] } deriving (Show, Eq)

instance Machine MockMachine where
  type V MockMachine = Value

  getVar x = do
    m <- S.get
    case M.lookup x (getMem m) of
      Just v -> return $ Happy v
      Nothing -> return $ Sad "variable not found"

  setVar x v = do
    m <- S.get
    S.put (m { getMem = M.insert x v (getMem m) })
    return $ Happy v

  inputVal = do
    m <- S.get
    case getInput m of
      (i:is) -> do
        S.put (m { getInput = is })
        return $ Happy i
      [] -> return $ Sad "end of input"

  outputVal v = do
    m <- S.get
    S.put (m { getOutput = getOutput m ++ [v] })
    return $ Happy v

  subVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 - v2))
  subVal _ _ = return $ Sad "Type error in subtraction"

  ltVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 < v2))
  ltVal _ _ = return $ Sad "Type error in <"

  gtVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 > v2))
  gtVal _ _ = return $ Sad "Type error in >"

  lteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 <= v2))
  lteVal _ _ = return $ Sad "Type error in <="

  gteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 >= v2))
  gteVal _ _ = return $ Sad "Type error in >="

  eqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal _ _ = return $ Sad "Type error in =="

  neqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal _ _ = return $ Sad "Type error in !="

  andVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 && v2))
  andVal _ _ = return $ Sad "Type error in &&"

  orVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 || v2))
  orVal _ _ = return $ Sad "Type error in ||"

  notVal (BoolVal v) = return $ Happy (BoolVal (not v))
  notVal _ = return $ Sad "Type error in !"

  selectValue (BoolVal True) c _ = c
  selectValue (BoolVal False) _ t = t
  selectValue (IntVal n) c t = if n /= 0 then c else t  -- backward compat, remove later

spec :: Spec
spec = do
  describe "reduceFully" $ do
    let initialMachine = MockMachine { getMem = M.empty, getInput = [], getOutput = [] }

    it "reduces a literal" $ do
      let term = Literal 10
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces a variable" $ do
      let term = Var "x"
      let machine = initialMachine { getMem = M.fromList [("x", IntVal 5)] }
      reduceFully term machine `shouldBe` (Right (IntVal 5), machine)

    it "reduces a let expression" $ do
      let term = Seq (Let "x" (Literal 5)) (Var "x")
      let finalMachine = initialMachine { getMem = M.fromList [("x", IntVal 5)] }
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), finalMachine)

    it "reduces a sequence" $ do
      let term = Seq (Let "x" (Literal 5)) (Var "x")
      let finalMachine = initialMachine { getMem = M.fromList [("x", IntVal 5)] }
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), finalMachine)

    it "reduces an if expression (then)" $ do
      let term = If (Literal 1) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces an if expression (else)" $ do
      let term = If (Literal 0) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 20), initialMachine)

    it "reduces a while loop" $ do
      let term = Seq (Let "x" (Literal 3)) (While (Var "x") (Let "x" (Sub (Var "x") (Literal 1))))
      let finalMachine = initialMachine { getMem = M.fromList [("x", IntVal 0)] }
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "reduces read and write" $ do
      let term = Seq (Read "x") (Write (Var "x"))
      let machine = initialMachine { getInput = [IntVal 42] }
      let finalMachine = machine { getMem = M.fromList [("x", IntVal 42)], getOutput = [IntVal 42], getInput = [] }
      reduceFully term machine `shouldBe` (Right (IntVal 42), finalMachine)

    it "reduces subtraction" $ do
      let term = Sub (Literal 10) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), initialMachine)

    it "reduces skip" $ do
      let term = Skip
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), initialMachine)
