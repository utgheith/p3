module Main (main) where

import FunSyntax (parse, prog)
import ParserCombinators (eof)

main :: IO ()
main = do
  text <- getContents
  let r = parse text $ do
        t <- prog
        _ <- eof
        return t
  print $ r
