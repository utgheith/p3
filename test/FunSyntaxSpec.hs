module FunSyntaxSpec (spec) where

import FunLexer (Token (..))
import FunSyntax (parse, prog)
import ParserCombinators (eof)
import Term (BinaryOp (..), Term (..))
import Test.Hspec

-- Helper function to parse from a string
parseString :: String -> Either String (Term, [Token])
parseString input = parse input $ do
  t <- prog
  _ <- eof
  return t

-- Helper function to parse from a file
parseFile :: FilePath -> IO (Either String (Term, [Token]))
parseFile path = do
  content <- readFile path
  return $ parseString content

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "basic literals" $ do
      it "parses integer literals" $ do
        let result = parse "42" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Literal 42, [])

      it "parses string literals" $ do
        let result = parse "\"hello\"" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (StringLiteral "hello", [])

      it "parses boolean literals" $ do
        let result = parse "true" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BoolLit True, [])

    describe "variables" $ do
      it "parses variable references" $ do
        let result = parse "x" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Var "x", [])

      it "parses variable assignments" $ do
        let result = parse "x = 42" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Let "x" (Literal 42), [])

    describe "arithmetic expressions" $ do
      it "parses addition" $ do
        let result = parse "x + y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Add (Var "x") (Var "y"), [])

      it "parses multiplication" $ do
        let result = parse "x * y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Mul (Var "x") (Var "y"), [])

      it "parses complex expressions with precedence" $ do
        let result = parse "x + y * z" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Add (Var "x") (BinaryOps Mul (Var "y") (Var "z")), [])

    describe "control flow" $ do
      it "parses if statements" $ do
        let result = parse "if (x > 0) { y = 1 } else { y = 0 }" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (If (BinaryOps Gt (Var "x") (Literal 0)) (Let "y" (Literal 1)) (Let "y" (Literal 0)), [])

      it "parses while loops" $ do
        let result = parse "while (x > 0) { x = x - 1 }" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (While (BinaryOps Gt (Var "x") (Literal 0)) (Let "x" (BinaryOps Sub (Var "x") (Literal 1))), [])

    describe "functions" $ do
      it "parses function definitions" $ do
        let result = parse "fun f() { print 42 }" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Let "f" (Fun [] (Write (Literal 42))), [])

      it "parses function calls" $ do
        let result = parse "f()" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (ApplyFun (Var "f") [], [])

      it "parses function calls with arguments" $ do
        let result = parse "f(x, y)" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (ApplyFun (Var "f") [Var "x", Var "y"], [])

    describe "tuples" $ do
      it "parses tuple creation" $ do
        let result = parse "x = [1, 2, 3]" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Let "x" (TupleTerm [Literal 1, Literal 2, Literal 3]), [])

      it "parses tuple access" $ do
        let result = parse "t[0]" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (AccessTuple (Var "t") (Literal 0), [])

    describe "error cases" $ do
      it "fails on invalid syntax" $ do
        let result = parse "x +" $ do
              t <- prog
              _ <- eof
              return t
        case result of
          Left _ -> True `shouldBe` True
          Right _ -> False `shouldBe` True

  describe "Integration tests" $ do
    it "parses a complete program" $ do
      let program = "x = 10\n y = x + 5\n print y"
      let result = parseString program
      case result of
        Left err -> fail $ "Parse error: " ++ err
        Right (ast, _) -> do
          ast `shouldSatisfy` \t -> case t of
            Seq _ _ -> True
            _ -> False

    it "parses from test files" $ do
      let testFiles = ["test/t1.fun"]
      mapM_ (\file -> do
        result <- parseFile file
        case result of
          Left err -> fail $ "Failed to parse " ++ file ++ ": " ++ err
          Right (ast, _) -> do
            ast `shouldSatisfy` (const True)  -- Just check it parses successfully
        ) testFiles

  describe "File-based tests" $ do
    it "can parse complex programs from files" $ do
      let complexProgram =
            unlines
              [ "var x = 10",
                "fun f(n) {",
                "    return n * 2",
                "}",
                "y = f(x)",
                "if (y > 15) {",
                "    print y",
                "} else {",
                "    print 0",
                "}"
              ]
      let result = parseString complexProgram
      case result of
        Left err -> fail $ "Parse error: " ++ err
        Right (ast, _) -> do
          ast `shouldSatisfy` (const True)
