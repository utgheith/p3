module Fun2Spec (spec) where

-- import qualified Term as T

import Control.Monad (forM_)
import Fun2 (Token (..), decompile, lexer, parser)
import ParserCombinators
import Test.Hspec

-- import qualified Control.Monad.State.Lazy as SM

parseExamples :: [String]
parseExamples =
  [ "",
    "1",
    "1 ; 2",
    "if 1 then 2 else 3",
    "{ 1 ; 2 ; 3 }",
    " 1+2",
    "(1+2)*3",
    "1+(2*3)",
    "((1+2) * (3+4))",
    "let x = 10; let y = 20"
  ]

spec :: Spec
spec = do
  describe "lexer" $ do
    it "handles an empty string" $ do
      parse lexer "" `shouldBe` Right []

    it "handles whitespace" $ do
      parse lexer "  \t\n  " `shouldBe` Right [TIgnore "  \t\n  "]

    it "lexes a number" $ do
      parse lexer "123" `shouldBe` Right [TNum 123]

    it "lexes an identifier" $ do
      parse lexer "abc" `shouldBe` Right [TIdent "abc"]

    it "lexes a special symbol" $ do
      parse lexer "=" `shouldBe` Right [TSpecial "="]

    it "lexes a multi-character special symbol" $ do
      parse lexer "<=" `shouldBe` Right [TSpecial "<="]

    it "lexes a simple expression" $ do
      parse lexer "x = 1" `shouldBe` Right [TIdent "x", TIgnore " ", TSpecial "=", TIgnore " ", TNum 1]

    it "handles a line comment" $ do
      parse lexer "// this is a comment\n" `shouldBe` Right [TIgnore "// this is a comment", TIgnore "\n"]

    it "handles a line comment without a newline" $ do
      parse lexer "// this is a comment" `shouldBe` Right [TIgnore "// this is a comment"]

    it "handles a line comment followed by integer" $ do
      parse lexer "// this is a comment\n123" `shouldBe` Right [TIgnore "// this is a comment", TIgnore "\n", TNum 123]

  describe "parser" $ do
    -- it ("parses an empty string") $ do
    --  SM.runStateT term [] `shouldBe` Right (T.Skip, [])

    forM_ parseExamples $ \input ->
      it ("parses: " ++ input) $ do
        let ast = parser input
        let input' = decompile ast
        let ast' = parser input'
        ast' `shouldBe` ast
        let input'' = decompile ast'
        input'' `shouldBe` input'
