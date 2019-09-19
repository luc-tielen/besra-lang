module Main ( main ) where

import Protolude hiding ( WriteMode )
import qualified Besra.ArgParser as ArgParser
import Besra.ArgParser hiding ( parse )
import qualified Besra.Repl as Repl
import Besra.Parser
import Besra.PrettyPrinter
import qualified Besra.Types.IR1 as IR1
import Besra.Types.Ann


type Module' = IR1.Module Parsed

main :: IO ()
main = do
  args <- getArgs
  ArgParser.parse args >>= \case
    Fmt fmtArgs -> fmt fmtArgs
    Repl -> Repl.run

fmt :: FmtArgs -> IO ()
fmt = \case
  FromStdIn output -> do
    let path = "<stdin>"
    contents <- getContents
    withAST path contents $ \parsed formatted ->
      case output of
        DoCheck -> checkFormatting path parsed formatted
        NoCheck -> putStrLn formatted
  FromFile path output -> do
    contents <- readFile path
    withAST path contents $ \parsed formatted ->
      case output of
        CheckMode -> checkFormatting path parsed formatted
        WriteMode Inplace -> writeFile path formatted
        WriteMode Stdout -> putStrLn formatted

withAST :: FilePath -> Text -> (ParseResult Module' -> Text -> IO ()) -> IO ()
withAST path contents f = do
  let parsed = parseFile path contents
  case parsed of
    Left err -> putStrLn $ formatError err
    Right ast -> do
      let formatted = prettyFormat ast
      f parsed formatted

checkFormatting :: FilePath -> ParseResult Module' -> Text -> IO ()
checkFormatting path parsed formatted = do
  let reparsed = parseFile (path <> " (after formatting)") formatted
  if parsed == reparsed
    then exitSuccess
    else exitFailure

