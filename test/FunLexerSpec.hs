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

    it "handles an unexpected character" $ do
      lexer "@" `shouldBe` [Error "Unexpected character: @"]

    it "lexes Cons(a, Nil)" $ do
      lexer "Cons(a, Nil)"
        `shouldBe` [ Ident "Cons",
                     Symbol "(",
                     Ident "a",
                     Symbol ",",
                     Ident "Nil",
                     Symbol ")"
                   ]

    it "lexes Cons with whitespace" $ do
      lexer "Cons ( a , Nil )"
        `shouldBe` [ Ident "Cons",
                     Symbol "(",
                     Ident "a",
                     Symbol ",",
                     Ident "Nil",
                     Symbol ")"
                   ]

    it "lexes nested cons Cons(1, Cons(2, Nil))" $ do
      lexer "Cons(1, Cons(2, Nil))"
        `shouldBe` [ Ident "Cons",
                     Symbol "(",
                     Num 1,
                     Symbol ",",
                     Ident "Cons",
                     Symbol "(",
                     Num 2,
                     Symbol ",",
                     Ident "Nil",
                     Symbol ")",
                     Symbol ")"
                   ]

    it "lexes Nil as an identifier" $ do
      lexer "Nil" `shouldBe` [Ident "Nil"]
