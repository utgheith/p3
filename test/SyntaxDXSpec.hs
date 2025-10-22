{-# LANGUAGE OverloadedStrings #-}

module SyntaxDXSpec where

import ParserCombinators (parseTerm)
import Sprintf (renderError)
import Test.Hspec

spec :: Spec
spec = do
  describe "boolean operators" $ do
    it "accepts symbolic && || !" $ do
      parseTerm "!(a && b) || c" `shouldSatisfy` isRight

    it "rejects textual and/or/not" $ do
      parseTerm "not (a and b) or c"
        `shouldSatisfy` isLeft

  describe "inequality spelling" $ do
    it "accepts !=" $ do
      parseTerm "a != b" `shouldSatisfy` isRight

    it "rejects /=" $ do
      parseTerm "a /= b" `shouldSatisfy` isLeft

  describe "bare equals in expressions" $ do
    it "rejects lone '='" $ do
      case parseTerm "1 = 2" of
        Left e -> renderError e `shouldContain` "Use '==' for equality"
        Right _ -> expectationFailure "expected an error"

  describe "keywords cannot be bound" $ do
    it "rejects let if = 1" $ do
      parseTerm "let if = 1 in if"
        `shouldSatisfy` isLeft
