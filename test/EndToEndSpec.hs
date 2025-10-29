{-# LANGUAGE OverloadedStrings #-}

module EndToEndSpec (spec) where

import Data.List (intercalate)
import Data.Text.Lazy (unpack)
import Decompile (decompile)
import FunSyntax (parse, prog)
import Result (Result (..))
import Simulator (Simulator (..), run)
import Sprintf (sprintf)
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, takeExtension, (</>))
import Term (Term (..))
import Test.Hspec
import Text.Pretty.Simple (pShowNoColor)
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
    oneFile fileName =
      let outFile = replaceExtension fileName ".out"
          debugFile = replaceExtension fileName ".debug"
          okFile = dir </> replaceExtension fileName ".ok"
          debug :: String -> String -> IO ()
          debug s x = do
            appendFile debugFile $ sprintf "\n:::::::: %s ::::::::\n" [s]
            appendFile debugFile x
       in do
            writeFile debugFile ""
            code <- readFile $ dir </> fileName
            debug "code" code
            let ast = compile code
            debug "ast" (unpack $ pShowNoColor ast)
            let code' = decompile ast
            debug "code'" code'
            let ast' = compile code'
            debug "ast'" (unpack $ pShowNoColor ast')
            -- ast' `shouldBe` ast
            let code'' = decompile ast'
            debug "code''" code''
            code'' `shouldBe` code'
            let ast'' = compile code''
            debug "ast''" (unpack $ pShowNoColor ast'')
            ast'' `shouldBe` ast'
            let out = run ast []
            debug "out" (unpack $ pShowNoColor out)
            writeFile outFile (display out)
            expectedOut <- readFile okFile
            display out `shouldBe` expectedOut
