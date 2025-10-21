module FunLexerSpec (spec) where

import FunLexer
import Test.Hspec

spec :: Spec
spec = do
  describe "lexer" $ do
    it "handles an empty string" $ do
      lexer "" `shouldBe` []

    it "handles whitespace" $ do
      lexer "  \t\n  " `shouldBe` []

    it "lexes a number" $ do
      lexer "123" `shouldBe` [Num 123]

    it "lexes an identifier" $ do
      lexer "abc" `shouldBe` [Ident "abc"]

    it "lexes a keyword" $ do
      lexer "if" `shouldBe` [Keyword "if"]

    it "lexes a symbol" $ do
      lexer "=" `shouldBe` [Symbol "="]

    it "lexes a multi-character symbol" $ do
      lexer "<=" `shouldBe` [Symbol "<="]

    it "lexes a simple expression" $ do
      lexer "var x = 1" `shouldBe` [Keyword "var", Ident "x", Symbol "=", Num 1]

    it "lexes a string" $ do
      lexer "\"hello\"" `shouldBe` [StringLiteralLexed "hello"]

    it "lexes a string with single quotes" $ do
      lexer "'\"'" `shouldBe` [StringLiteralLexed "\""]

    it "handles an unexpected character" $ do
      lexer "@" `shouldBe` [Error "Unexpected character: @"]

    -- DX: \'#' comments are ignored as whitespace
    it "ignores # line comments" $ do
      lexer "+   # comment\n   *"
        `shouldBe` [Symbol "+", Symbol "*"]

    -- DX: keywords don't split identifiers (no 'if' + 'x' for 'ifx')
    it "doesn't split keyword prefixes inside identifiers" $ do
      lexer "ifx <= 1" `shouldBe` [Ident "ifx", Symbol "<=", Num 1]
