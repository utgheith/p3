module Runner (runner) where

import Data.Text.Lazy (unpack)
import Decompile (decompile)
import FunSyntax (parse, prog)
import Result (Result (..))
import Simulator (Simulator (..), run)
import System.IO (Handle, hFlush, hPutStrLn)
import Term (Term (..))
import Text.Pretty.Simple (pShowNoColor)
import Typer (typer)
import Value (displayValue)

-- Runs the full (parse -> decompile)* -> typecheck -> simulate pipeline, with debugging output

runner :: Handle -> Handle -> String -> IO ()
runner outHandle debugHandle code = do
  debugStr "code" code

  -- do 3 iterations of parse -> decompile
  term_or_error <- compile Nothing 3 code
  case term_or_error of
    Left err -> do
      hPutStrLn outHandle "?compiler error"
      debug "compiler error" err
    Right t -> do
      type_or_error <- typer debugHandle t
      hPutStrLn debugHandle "\n--- Typechecking complete ---"
      hPutStrLn debugHandle (unpack (pShowNoColor type_or_error))
      case type_or_error of
        Left err -> do
          hPutStrLn outHandle "?type error"
          debug "type error" err
        Right tType -> do
          debugStr "type" (unpack (pShowNoColor tType))
          case run t [] of
            (Left err, simulator) -> do
              hPutStrLn outHandle "?runtime error"
              debugStr "runtime error" err
              debugSim simulator
            (Right val, s@(Simulator _ _ out)) -> do
              mapM_ (hPutStrLn outHandle . displayValue) out
              hPutStrLn outHandle $ "Final Value: " ++ displayValue val
              debugSim s
  where
    debugStr what v = do
      hPutStrLn debugHandle ("\n--- " ++ what ++ " ---")
      hPutStrLn debugHandle v
      hFlush debugHandle

    debug what v = do
      debugStr what (unpack (pShowNoColor v) ++ "\n")

    debugSim = debug "Simulator State"

    compile :: Maybe Term -> Int -> String -> IO (Either String Term)
    compile prevTerm phase code' = do
      debugStr "compilation phase" $ show phase
      debugStr "code" code'
      case parse code' prog of
        Ok (ast, []) -> do
          _ <- debug "AST" ast
          case prevTerm of
            Just oldAst
              | ast /= oldAst && (phase == 0) ->
                  return $ Left "AST changed between compilation phases"
            _ ->
              if phase == 0
                then return $ Right ast
                else compile (Just ast) (phase - 1) (decompile ast)
        Ok (_, left) -> return $ Left $ "Parsing incomplete, leftover tokens: " ++ show left
        Err err -> return $ Left $ "Parsing failed with: " ++ err
