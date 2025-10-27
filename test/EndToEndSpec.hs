module EndToEndSpec (spec) where

import Decompile (decompile)
import FunSyntax (parse, prog)
import Result (Result (..))
import Term (Term (..))
import Test.Hspec

fileNames :: [String]
fileNames =
  [ "test_files/t1.fun",
    "test_files/t2.fun"
  ]

compile :: String -> Term
compile code = case parse code prog of
  Ok (ast, []) -> ast
  Ok (_, left) -> error $ "Parsing incomplete, leftover tokens: " ++ show left
  Err err -> error $ "Parsing failed with: " ++ err

spec :: Spec
spec = do
  describe "End to end tests" $ do
    mapM_ (it "parses successfully" . oneFile) fileNames
  where
    oneFile fileName = do
      code <- readFile fileName
      let ast = compile code
      let code' = decompile ast
      putStrLn code'
      let ast' = compile code'
      ast' `shouldBe` ast
      let code'' = decompile ast'
      code'' `shouldBe` code'
