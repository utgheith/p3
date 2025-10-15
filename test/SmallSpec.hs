
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}

module SmallSpec (spec) where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import Small
import Term
import Test.Hspec

-- A mock machine for testing
data MockMachine = MockMachine { getMem :: M.Map String Integer, getInput :: [Integer], getOutput :: [Integer] } deriving (Show, Eq)

instance Machine MockMachine where
  type V MockMachine = Integer

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

  subVal v1 v2 = return $ Happy (v1 - v2)
  addVal v1 v2 = return $ Happy (v1 + v2)
  mulVal v1 v2 = return $ Happy (v1 * v2)
  divVal v1 v2 = if v2 == 0 then return $ Sad "Cannot divide by 0" else
        return $ Happy (v1 `div` v2) -- I don't want the actual interpreter to crash
  modVal v1 v2 = if v2 == 0 then return $ Sad "Cannot mod by 0" else
        return $ Happy (v1 `mod` v2) -- I don't want the actual interpreter to crash

  selectValue v c t = if v /= 0 then c else t

  intToV _ n = n
  vToInt _ v = v

spec :: Spec
spec = do
  describe "reduceFully" $ do
    let initialMachine = MockMachine { getMem = M.empty, getInput = [], getOutput = [] }

    it "reduces a literal" $ do
      let term = Literal 10
      reduceFully term initialMachine `shouldBe` (Right 10, initialMachine)

    it "reduces a variable" $ do
      let term = Var "x"
      let machine = initialMachine { getMem = M.fromList [("x", 5)] }
      reduceFully term machine `shouldBe` (Right 5, machine)

    it "reduces a let expression" $ do
      let term = Seq (Let "x" (Literal 5)) (Var "x")
      let finalMachine = initialMachine { getMem = M.fromList [("x", 5)] }
      reduceFully term initialMachine `shouldBe` (Right 5, finalMachine)

    it "reduces a sequence" $ do
      let term = Seq (Let "x" (Literal 5)) (Var "x")
      let finalMachine = initialMachine { getMem = M.fromList [("x", 5)] }
      reduceFully term initialMachine `shouldBe` (Right 5, finalMachine)

    it "reduces an if expression (then)" $ do
      let term = If (Literal 1) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right 10, initialMachine)

    it "reduces an if expression (else)" $ do
      let term = If (Literal 0) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right 20, initialMachine)

    it "reduces a while loop" $ do
      let term = Seq (Let "x" (Literal 3)) (While (Var "x") (Let "x" (BinaryOps Sub (Var "x") (Literal 1))))
      let finalMachine = initialMachine { getMem = M.fromList [("x", 0)] }
      reduceFully term initialMachine `shouldBe` (Right 0, finalMachine)

    it "reduces read and write" $ do
      let term = Seq (Read "x") (Write (Var "x"))
      let machine = initialMachine { getInput = [42] }
      let finalMachine = machine { getMem = M.fromList [("x", 42)], getOutput = [42], getInput = [] }
      reduceFully term machine `shouldBe` (Right 42, finalMachine)

    it "reduces subtraction" $ do
      let term = BinaryOps Sub (Literal 10) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right 7, initialMachine)

    it "reduces addition" $ do
      let term = BinaryOps Add (Literal 10) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right 13, initialMachine)

    it "reduces multiplication" $ do
      let term = BinaryOps Mul (Literal 10) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right 30, initialMachine)

    it "reduces division - nonzero denominator case" $ do
      let term = BinaryOps Div (Literal 12) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right 4, initialMachine)

    it "reduces division - zero denominator case" $ do
      let term = BinaryOps Div (Literal 12) (Literal 0)
      reduceFully term initialMachine `shouldBe` (Left "Cannot divide by 0", initialMachine)

    it "reduces modulus - nonzero denominator case" $ do
      let term = BinaryOps Mod (Literal 12) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right 0, initialMachine)

    it "reduces modulus - zero denominator case" $ do
      let term = BinaryOps Mod (Literal 12) (Literal 0)
      reduceFully term initialMachine `shouldBe` (Left "Cannot mod by 0", initialMachine)

    it "reduces skip" $ do
      let term = Skip
      reduceFully term initialMachine `shouldBe` (Right 0, initialMachine)
