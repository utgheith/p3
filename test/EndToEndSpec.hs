module EndToEndSpec (spec) where

import Decompile (decompile)
import FunSyntax (parse, prog)
import Result (Result (..))
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Term (Term (..))
import Test.Hspec

compile :: String -> Term
compile code = case parse code prog of
  Ok (ast, []) -> ast
  Ok (_, left) -> error $ "Parsing incomplete, leftover tokens: " ++ show left
  Err err -> error $ "Parsing failed with: " ++ err

dir :: String
dir = "test_files/"

spec :: Spec
spec = do
  describe "End to end tests" $ do
    entries <- runIO $ listDirectory "test_files"
    let fileNames = [entry | entry <- entries, takeExtension entry == ".fun"]
    mapM_ (\fileName -> it fileName (oneFile fileName)) fileNames
  where
    oneFile fileName = do
      code <- readFile $ dir </> fileName
      let ast = compile code
      let code' = decompile ast
      _ <- putStrLn code'
      let ast' = compile code'
      ast' `shouldBe` ast
      let code'' = decompile ast'
      code'' `shouldBe` code'
