
module ParserCombinatorsSpec (spec) where

import ParserCombinators
import Control.Monad.State.Lazy (runStateT)
import Test.Hspec

spec :: Spec
spec = do
  describe "eof" $ do
    it "succeeds on empty input" $ do
      runStateT (eof :: Parser Int ()) [] `shouldBe` Right ((), [])
    it "fails on non-empty input" $ do
      runStateT (eof :: Parser Int ()) [1] `shouldBe` Left "expected eof but found: [1]"

  describe "satisfy" $ do
    it "succeeds when predicate is met" $ do
      let p = satisfy (\x -> if x > 0 then Just x else Nothing)
      runStateT (p :: Parser Int Int) [1, 2] `shouldBe` Right (1, [2])
    it "fails when predicate is not met" $ do
      let p = satisfy (\x -> if x > 0 then Just x else Nothing)
      runStateT (p :: Parser Int Int) [-1, 2] `shouldBe` Left "failed to satisfy predicate at [-1,2]"

  describe "token" $ do
    it "consumes the correct token" $ do
      runStateT (token 1) [1, 2] `shouldBe` Right (1, [2])
    it "fails on incorrect token" $ do
      runStateT (token 1) [2, 1] `shouldBe` Left "expected 1, found [2,1]"

  describe "<|>" $ do
    it "chooses the first parser if it succeeds" $ do
      let p = token 1 <|> token 2
      runStateT (p :: Parser Int (Either Int Int)) [1, 2] `shouldBe` Right (Left 1, [2])
    it "chooses the second parser if the first fails" $ do
      let p = token 1 <|> token 2
      runStateT (p :: Parser Int (Either Int Int)) [2, 1] `shouldBe` Right (Right 2, [1])

  describe "oneof" $ do
    it "picks the first successful parser" $ do
      let p = oneof [token 1, token 2, token 3]
      runStateT (p :: Parser Int Int) [2, 3] `shouldBe` Right (2, [3])

  describe "opt" $ do
    it "returns Just the value if parser succeeds" $ do
      runStateT (opt (token 1)) [1, 2] `shouldBe` Right (Just 1, [2])
    it "returns Nothing if parser fails" $ do
      runStateT (opt (token 1)) [2, 1] `shouldBe` Right (Nothing, [2, 1])

  describe "rpt" $ do
    it "parses zero or more occurrences" $ do
      runStateT (rpt (token 1)) [1, 1, 2] `shouldBe` Right ([1, 1], [2])
      runStateT (rpt (token 1)) [2, 1, 1] `shouldBe` Right ([], [2, 1, 1])

  describe "rptSep" $ do
    it "parses zero or more occurrences with a separator" $ do
      let p = rptSep (token 1) (token 0)
      runStateT p [1, 0, 1, 2] `shouldBe` Right (Just (1, [(0, 1)]), [2])
      runStateT p [2, 1, 0] `shouldBe` Right (Nothing, [2, 1, 0])

  describe "rptDropSep" $ do
    it "parses zero or more occurrences and drops the separator" $ do
      let p = rptDropSep (token 1) (token 0)
      runStateT p [1, 0, 1, 2] `shouldBe` Right ([1, 1], [2])
      runStateT p [2, 1, 0] `shouldBe` Right ([], [2, 1, 0])
