module ParserCombinatorsSpec (spec) where

import Control.Monad.State.Lazy (runStateT)
import Data.Char (isDigit)
import Data.Functor (($>))
import ParserCombinators
import Test.Hspec

spec :: Spec
spec = do
  describe "eof" $ do
    it "succeeds on empty input" $
      runStateT (eof :: Parser Int ()) [] `shouldBe` Right ((), [])
    it "fails on non-empty input" $
      runStateT (eof :: Parser Int ()) [1] `shouldBe` Left "expected eof but found: [1]"

  describe "satisfy" $ do
    it "succeeds when predicate is met" $ do
      let p = satisfy (\x -> if x > 0 then Just x else Nothing)
      runStateT (p :: Parser Int Int) [1, 2] `shouldBe` Right (1, [2])
    it "fails when predicate is not met" $ do
      let p = satisfy (\x -> if x > 0 then Just x else Nothing)
      runStateT (p :: Parser Int Int) [-1, 2] `shouldBe` Left "failed to satisfy predicate at [-1,2]"

  describe "token" $ do
    it "consumes the correct token" $
      runStateT (token '1') ['1', '2'] `shouldBe` Right ('1', ['2'])
    it "fails on incorrect token" $
      runStateT (token '1') ['2', '1'] `shouldBe` Left "expected '1', found \"21\""

  describe "<|>" $ do
    it "chooses the first parser if it succeeds" $ do
      let p = token 1 <|> token 2
      runStateT (p :: Parser Int Int) [1, 2] `shouldBe` Right (1, [2])
    it "chooses the second parser if the first fails" $ do
      let p = token 1 <|> token 2
      runStateT (p :: Parser Int Int) [2, 1] `shouldBe` Right (2, [1])

  describe "alt" $ do
    let p1 = token 1 :: Parser Int Int
    let p2 = satisfy (\x -> if x > 5 then Just (show x) else Nothing) :: Parser Int String
    let p = alt p1 p2
    it "returns Left if the first parser succeeds" $
      runStateT p [1, 2] `shouldBe` Right (Left 1, [2])
    it "returns Right if the first parser fails and second succeeds" $
      runStateT p [10, 1] `shouldBe` Right (Right "10", [1])

  describe "oneof" $ do
    it "picks the first successful parser" $ do
      let p = oneof [token 1, token 2, token 3]
      runStateT (p :: Parser Int Int) [2, 3] `shouldBe` Right (2, [3])
    it "fails if all parsers fail" $ do
      let p = oneof [token 1, token 2]
      runStateT (p :: Parser Int Int) [3, 4] `shouldBe` Left "no rule matched"

  describe "opt" $ do
    it "returns Just the value if parser succeeds" $
      runStateT (opt (token '1')) ['1', '2'] `shouldBe` Right (Just '1', ['2'])
    it "returns Nothing if parser fails" $
      runStateT (opt (token '1')) ['2', '1'] `shouldBe` Right (Nothing, ['2', '1'])

  describe "rpt" $ do
    it "parses zero or more occurrences" $ do
      runStateT (rpt (token '1')) ['1', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
      runStateT (rpt (token '1')) ['2', '1', '1'] `shouldBe` Right ([], ['2', '1', '1'])
    it "works with alternatives" $
      runStateT (rpt (token '1' <|> token '2')) ['2', '1', '3'] `shouldBe` Right (['2', '1'], ['3'])

  describe "rptSep" $ do
    it "parses zero or more occurrences with a separator" $ do
      let p = rptSep (token '1') (token '0')
      runStateT p ['1', '0', '1', '2'] `shouldBe` Right (Just ('1', [('0', '1')]), ['2'])
      runStateT p ['2', '1', '0'] `shouldBe` Right (Nothing, ['2', '1', '0'])
    it "works with alternatives" $ do
      let p = rptSep (token '1' <|> token '2') (token '3' <|> token '4')
      runStateT p ['2', '3', '1', '4', '2'] `shouldBe` Right (Just ('2', [('3', '1'), ('4', '2')]), [])

  describe "rptDropSep" $ do
    it "parses zero or more occurrences and drops the separator" $ do
      let p = rptDropSep (token '1') (token '0')
      runStateT p ['1', '0', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
      runStateT p ['2', '1', '0'] `shouldBe` Right ([], ['2', '1', '0'])
    it "works with alternatives" $ do
      let p = rptDropSep (token '1' <|> token '2') (token '3' <|> token '4')
      runStateT p ['2', '3', '1', '4', '2'] `shouldBe` Right (['2', '1', '2'], [])

  describe "some" $ do
    it "parses one or more occurrences" $
      runStateT (some (token '1')) ['1', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
    it "fails if no occurrences found" $
      case runStateT (some (token '1')) ['2', '1', '1'] of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected failure but got success"

  describe "sepBy" $ do
    it "parses zero or more occurrences separated by a separator" $ do
      let p = sepBy (token '1') (token '0')
      runStateT p ['1', '0', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
      runStateT p ['2', '1', '0'] `shouldBe` Right ([], ['2', '1', '0'])
    it "stops if there are two consecutive separators" $ do
      let p = sepBy (token '1') (token '0')
      runStateT p ['1', '0', '0', '1'] `shouldBe` Right (['1'], ['0', '0', '1'])

  describe "sepBy1" $ do
    it "parses one or more occurrences separated by a separator" $ do
      let p = sepBy1 (token '1') (token '0')
      runStateT p ['1', '0', '1', '2'] `shouldBe` Right (['1', '1'], ['2'])
      runStateT p ['1', '2'] `shouldBe` Right (['1'], ['2'])
    it "fails if no occurrences found" $
      case runStateT (sepBy1 (token '1') (token '0')) ['2', '1', '0'] of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected failure but got success"

  describe "between" $ do
    it "parses content between delimiters" $ do
      let p = between (token '(') (token ')') (token 'x')
      runStateT p ['(', 'x', ')', 'y'] `shouldBe` Right ('x', ['y'])
    it "parses identical open and close delimiters" $ do
      let p = between (token '$') (token '$') (token 'x')
      runStateT p ['$', 'x', '$', 'y', '$'] `shouldBe` Right ('x', ['y', '$'])

  describe "skip" $ do
    it "ignores the result of a parser" $ do
      let p = skip (token '1')
      runStateT p ['1', '2'] `shouldBe` Right ((), ['2'])
    it "errors if the pattern is not found" $ do
      let p = skip (token '1' <|> token '2')
      runStateT p ['3', '2'] `shouldBe` Left "no rule matched"

  describe "lookAhead" $ do
    it "parses without consuming input" $ do
      let p = lookAhead (token '1')
      runStateT p ['1', '2'] `shouldBe` Right ('1', ['1', '2'])
    it "errors if the pattern ahead is not found" $ do
      let p = lookAhead (token '1' <|> token '2')
      runStateT p ['3', '2'] `shouldBe` Left "no rule matched"

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
    let num = satisfy (\c -> if isDigit c then Just (fromEnum c - fromEnum '0') else Nothing)
    let plus = token '+' $> (+)
    let p = chainl1 num plus
    it "parses left-associative expressions" $
      runStateT p ['1', '+', '2', '+', '3'] `shouldBe` Right (6, [])
    it "handles single value without operators" $
      runStateT p ['5'] `shouldBe` Right (5, [])

  describe "chainr1" $ do
    let num = satisfy (\c -> if isDigit c then Just (fromEnum c - fromEnum '0') else Nothing)
    let minus = token '-' $> (-)
    let p = chainr1 num minus
    it "parses right-associative expressions" $
      -- 5 - 3 - 1 should be parsed as 5 - (3 - 1) = 3
      runStateT p ['5', '-', '3', '-', '1'] `shouldBe` Right (3, [])
    it "handles single value without operators" $
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
      let p = tokens ""
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
