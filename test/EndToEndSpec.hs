module EndToEndSpec (spec) where

import Data.List (intercalate)
import Decompile (decompile)
import FunSyntax (parse, prog)
import Result (Result (..))
import Simulator (Simulator (..), run)
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, takeExtension, (</>))
import Term (Term (..))
import Test.Hspec
import Value (Value (..))

displayValue :: Value -> String
displayValue (IntVal n) = show n
displayValue (BoolVal b) = show b
displayValue (StringVal s) = show s
displayValue UnitVal = "()"
displayValue (Tuple vals) = "(" ++ intercalate ", " (map displayValue vals) ++ ")"
displayValue (ClosureVal {}) = "<closure>"
displayValue (Dictionary {}) = "<dictionary>"

display :: (Either String Value, Simulator) -> String
display (Left err, _) = "Error: " ++ err
display (Right val, Simulator _ _ out) = intercalate "\n" ((displayValue <$> out) ++ ["Result: " ++ displayValue val])

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
      let out = run ast []
      let outFile = replaceExtension fileName ".out"
      let debugFile = replaceExtension fileName ".debug"
      let okFile = dir </> replaceExtension fileName ".ok"
      writeFile debugFile (show out)
      writeFile outFile (display out)
      expectedOut <- readFile okFile
      display out `shouldBe` expectedOut
