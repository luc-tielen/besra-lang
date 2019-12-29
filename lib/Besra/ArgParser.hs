
module Besra.ArgParser
  ( parse
  , Args(..)
  , FmtArgs(..)
  , FmtOutputMode(..)
  , WriteOpts(..)
  , CheckOpts(..)
  ) where

import Protolude hiding ( WriteMode )
import Prelude ( String )
import Options.Applicative


data Args
  = Fmt FmtArgs
  | TypeCheck TypeCheckArgs
  | Repl
  deriving (Eq, Show)

-- | Data type representing how formatter should behave.
data FmtArgs
  = FromStdIn CheckOpts
  -- ^ Reads from stdin, can only output result via stdout.
  | FromFile FilePath FmtOutputMode  -- TODO support directories
  -- ^ Reads from file, can modify inplace or output to stdout
  deriving (Eq, Show)

type TypeCheckArgs = FilePath  -- ^ Typechecks a specific file.

-- | Data type describing in which output mode the formatter should be configured.
data FmtOutputMode
  = CheckMode            -- ^ Mode for checking if a file is already formatted.
  | WriteMode WriteOpts  -- ^ Mode for writing formatted output
  deriving (Eq, Show)

-- | Data type describing if formatter should check if file is formatted or not.
data CheckOpts = DoCheck | NoCheck
  deriving (Eq, Show)

-- | Data type describing where to write data to.
data WriteOpts
  = Stdout            -- ^ Write to standard output
  | Inplace           -- ^ Edit file in place
  deriving (Eq, Show)


parse :: [String] -> IO Args
parse = handleParseResult . execParserPure parserPrefs parserInfo
  where desc = fullDesc <> progDesc "CLI interface to the Besra compiler"
        parserPrefs = prefs $ showHelpOnError <> showHelpOnEmpty
        parserInfo = info (parser <**> helper) desc

parser :: Parser Args
parser = hsubparser $ fmtCommand <> tcCommand <> replCommand
  where fmtCommand = command "fmt" $ info fmtParser fmtDesc
        fmtDesc = fullDesc
                <> header "besra fmt - a pretty printer/formatter for Besra files"
                <> progDesc "Formats input files."
        replCommand = command "repl" $ info replParser replDesc
        replDesc = fullDesc
                 <> header "besra repl - REPL for the Besra language"
                 <> progDesc "Opens up a Read Eval Print Loop."
        tcCommand = command "typecheck" $ info tcParser tcDesc
        tcDesc = fullDesc
               <> header "besra typecheck - typecheck a file."
               <> progDesc "Checks and outputs any type errors in the file if present."

fmtParser :: Parser Args
fmtParser = Fmt <$> fmtParser' where
  fmtParser' = FromFile <$> inputFile <*> outputMode
            <|> stdInput <*> checkMode
  inputFile = argument str
    (metavar "FILE"
    <> help "Which FILE to read; if none provided, tries to read from stdin."
    )
  stdInput = flag' FromStdIn
    (long "stdin" <> help "Whether input should be read from standard input.")
  outputMode = checkMode' <|> writeMode

  checkModeConfig =
    long "check"
    <> help ("Whether check for already formatted file needs to be performed?"
          <> "If checking, exits with 0 if already formatted, otherwise 1.")

  checkMode' = flag' CheckMode checkModeConfig
  checkMode = flag NoCheck DoCheck checkModeConfig
  writeMode = WriteMode <$> flag Inplace Stdout
    (long "stdout"
    <> help ("Whether formatted output should be written to standard output "
          <> "or modified inplace. Defaults to inplace modification.")
    )

tcParser :: Parser Args
tcParser = TypeCheck <$> inputFile where
  inputFile = argument str (metavar "FILE" <> help "Which FILE to typecheck.")

replParser :: Parser Args
replParser = pure Repl

