module Main (main) where

import FunSyntax (parse, prog)
import Result (Result (..))
import Simulator (run)

main :: IO ()
main = do
  code <- getContents
  let out = case parse code prog of
        Ok (t, []) -> run t []
        Ok (_, ts) -> error $ "Unconsumed tokens: " ++ show ts
        Err e -> error $ "Parse error: " ++ show e

  print out
