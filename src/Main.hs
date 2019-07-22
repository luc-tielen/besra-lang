module Main ( main ) where

import Protolude hiding ( WriteMode )
import Besra.ArgParser
import Besra.Parser
import Besra.PrettyPrinter
import Besra.Types.IR1.Module
import Besra.Types.Ann


type Module' = Module 'Parsed

main :: IO ()
main = parseArgs >>= \case
  Fmt fmtArgs -> fmt fmtArgs

fmt :: FmtArgs -> IO ()
fmt args@(FmtArgs input output) = do
  validateFmtArgs args
  (path, contents) <- readInput input
  let parsed = parseFile path contents
  case parsed of
    Left err -> putStr $ formatError err
    Right ast -> do
      let formatted = prettyPrint ast
      case output of
        CheckMode -> checkFormatting path parsed formatted
        WriteMode Inplace -> writeFile path formatted
        WriteMode Stdout -> putStrLn formatted

-- TODO remove after refactoring type of fmtargs, fix other open TODOs
validateFmtArgs :: FmtArgs -> IO ()
validateFmtArgs (FmtArgs input output) =
  if input == Stdin && output == WriteMode Inplace
    then panic "Not allowed to format inplace when reading from stdin"
    else pure ()

readInput :: FmtInputMode -> IO (FilePath, Text)
readInput input =
  case input of
    Stdin -> ("<stdin>", ) <$> getContents
    InputFile path -> (path, ) <$> readFile path

checkFormatting :: FilePath -> ParseResult Module' -> Text -> IO ()
checkFormatting path parsed formatted = do
  let reparsed = parseFile (path <> " (after formatting)") formatted
  if parsed == reparsed
    then exitSuccess
    else exitFailure

