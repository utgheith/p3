module Main (main) where

import Runner (runner)
import System.IO (IOMode (..), hClose, openFile, stdout)

main :: IO ()
main = do
  code <- getContents
  debugFile <- openFile "sim.debug" WriteMode
  _ <- runner stdout debugFile code
  hClose debugFile
