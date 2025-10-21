module ParserCombinatorsSpec (spec) where

import Control.Monad.State.Lazy (runStateT)
import Data.Char (isDigit)
import Data.Functor (($>))
import ParserCombinators
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
      runStateT (token '1') ['1', '2'] `shouldBe` Right ('1', ['2'])
    it "fails on incorrect token" $ do
      runStateT (token '1') ['2', '1'] `shouldBe` Left "expected '1', found \"21\""

  describe "<|>" $ do
    it "chooses the first parser if it succeeds" $ do
      let p = token 1 <|> token 2
      runStateT (p :: Parser Int Int) [1, 2] `shouldBe` Right (1, [2])
    it "chooses the second parser if the first fails" $ do
      let p = token 1 <|> token 2
      runStateT (p :: Parser Int Int) [2, 1] `shouldBe` Right (2, [1])

  describe "alt" $ do
    it "returns Left if the first parser succeeds" $ do
      let p1 = token 1 :: Parser Int Int
      let p2 = satisfy (\x -> if x > 5 then Just (show x) else Nothing) :: Parser Int String
      let p = alt p1 p2
      runStateT p [1, 2] `shouldBe` Right (Left 1, [2])
    it "returns Right if the first parser fails and second succeeds" $ do
      let p1 = token 1 :: Parser Int Int
      let p2 = satisfy (\x -> if x > 5 then Just (show x) else Nothing) :: Parser Int String
      let p = alt p1 p2
      runStateT p [10, 1] `shouldBe` Right (Right "10", [1])

  describe "oneof" $ do
    it "picks the first successful parser" $ do
      let p = oneof [token 1, token 2, token 3]
      runStateT (p :: Parser Int Int) [2, 3] `shouldBe` Right (2, [3])

  describe "opt" $ do
    it "returns Just the value if parser succeeds" $ do
      runStateT (opt (token '1')) ['1', '2'] `shouldBe` Right (Just '1', ['2'])
    it "returns Nothing if parser fails" $ do
      runStateT (opt (token '1')) ['2', '1'] `shouldBe` Right (Nothing, ['2', '1'])

  describe "rpt" $ do
    it "parses zero or more occurrences" $ do
      runStateT (rpt (token '1')) ['1', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
      runStateT (rpt (token '1')) ['2', '1', '1'] `shouldBe` Right ([], ['2', '1', '1'])

  describe "rptSep" $ do
    it "parses zero or more occurrences with a separator" $ do
      let p = rptSep (token '1') (token '0')
      runStateT p ['1', '0', '1', '2'] `shouldBe` Right (Just ('1', [('0', '1')]), ['2'])
      runStateT p ['2', '1', '0'] `shouldBe` Right (Nothing, ['2', '1', '0'])

  describe "rptDropSep" $ do
    it "parses zero or more occurrences and drops the separator" $ do
      let p = rptDropSep (token '1') (token '0')
      runStateT p ['1', '0', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
      runStateT p ['2', '1', '0'] `shouldBe` Right ([], ['2', '1', '0'])

  describe "some" $ do
    it "parses one or more occurrences" $ do
      runStateT (some (token '1')) ['1', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
    it "fails if no occurrences found" $ do
      case runStateT (some (token '1')) ['2', '1', '1'] of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected failure but got success"

  describe "sepBy" $ do
    it "parses zero or more occurrences separated by a separator" $ do
      let p = sepBy (token '1') (token '0')
      runStateT p ['1', '0', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
      runStateT p ['2', '1', '0'] `shouldBe` Right ([], ['2', '1', '0'])
    it "does not consume a leading separator when no item precedes it" $ do
      let p = sepBy (token '1') (token '0')
      runStateT p ['0','1'] `shouldBe` Right ([], ['0','1'])

  describe "sepBy1" $ do
    it "parses one or more occurrences separated by a separator" $ do
      let p = sepBy1 (token '1') (token '0')
      runStateT p ['1', '0', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
      runStateT p ['1', '2'] `shouldBe` Right (['1'], ['2'])
    it "fails if no occurrences found" $ do
      case runStateT (sepBy1 (token '1') (token '0')) ['2', '1', '0'] of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected failure but got success"

  describe "between" $ do
    it "parses content between delimiters" $ do
      let p = between (token '(') (token ')') (token 'x')
      runStateT p ['(', 'x', ')', 'y'] `shouldBe` Right ('x', ['y'])

  describe "skip" $ do
    it "ignores the result of a parser" $ do
      let p = skip (token '1')
      runStateT p ['1', '2'] `shouldBe` Right ((), ['2'])

  describe "lookAhead" $ do
    it "parses without consuming input" $ do
      let p = lookAhead (token '1')
      runStateT p ['1', '2'] `shouldBe` Right ('1', ['1', '2'])

  describe "parse" $ do
    it "runs parser and ensures EOF" $ do
      let p = token '1'
      parse p ['1'] `shouldBe` Right '1'
    it "fails if input remains after parsing" $ do
      let p = token '1'
      case parse p ['1', '2'] of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected failure but got success"

  describe "chainl1" $ do
    it "parses left-associative expressions" $ do
      let num = satisfy (\c -> if isDigit c then Just (fromEnum c - fromEnum '0') else Nothing)
      let plus = token '+' $> (+)
      let p = chainl1 num plus
      runStateT p ['1', '+', '2', '+', '3'] `shouldBe` Right (6, [])
    it "handles single value without operators" $ do
      let num = satisfy (\c -> if isDigit c then Just (fromEnum c - fromEnum '0') else Nothing)
      let plus = token '+' $> (+)
      let p = chainl1 num plus
      runStateT p ['5'] `shouldBe` Right (5, [])

  describe "chainr1" $ do
    it "parses right-associative expressions" $ do
      let num = satisfy (\c -> if isDigit c then Just (fromEnum c - fromEnum '0') else Nothing)
      let minus = token '-' $> (-)
      let p = chainr1 num minus
      -- 5 - 3 - 1 should be parsed as 5 - (3 - 1) = 3
      runStateT p ['5', '-', '3', '-', '1'] `shouldBe` Right (3, [])
    it "handles single value without operators" $ do
      let num = satisfy (\c -> if isDigit c then Just (fromEnum c - fromEnum '0') else Nothing)
      let minus = token '-' $> (-)
      let p = chainr1 num minus
      runStateT p ['5'] `shouldBe` Right (5, [])

  describe "tokens" $ do
    it "matches a sequence of tokens" $ do
      let p = tokens ['a', 'b', 'c']
      runStateT p ['a', 'b', 'c', 'd'] `shouldBe` Right (['a', 'b', 'c'], ['d'])
    it "fails if sequence doesn't match" $ do
      let p = tokens ['a', 'b', 'c']
      case runStateT p ['a', 'x', 'c'] of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected failure but got success"
    it "succeeds on empty sequence" $ do
      let p = tokens ([] :: [Char])
      runStateT p ['a', 'b'] `shouldBe` Right ([], ['a', 'b'])

  describe "string" $ do
    it "matches a string" $ do
      let p = string "hello"
      runStateT p "hello world" `shouldBe` Right ("hello", " world")
    it "fails if string doesn't match" $ do
      let p = string "hello"
      case runStateT p "help" of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected failure but got success"
