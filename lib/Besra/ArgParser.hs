
module Besra.ArgParser
  ( parseArgs
  , Args(..)
  , FmtArgs(..)
  , FmtInputMode(..)
  , FmtOutputMode(..)
  , WriteOpts(..)
  ) where

import Protolude hiding ( WriteMode )
import Options.Applicative


newtype Args = Fmt FmtArgs
  deriving (Eq, Show)

data FmtArgs = FmtArgs FmtInputMode FmtOutputMode
  deriving (Eq, Show)

-- | Data type describing where to find data to be formatted.
data FmtInputMode
  = Stdin               -- ^ Read from standard input.
  | InputFile FilePath  -- ^ Read from a file (TODO: also support directories)
  deriving (Eq, Show)

-- | Data type describing in which output mode the formatter should be configured.
data FmtOutputMode
  = CheckMode            -- ^ Mode for checking if a file is already formatted.
  | WriteMode WriteOpts  -- ^ Mode for writing formatted output
  deriving (Eq, Show)

-- | Data type describing where to write data to.
data WriteOpts
  = Stdout            -- ^ Write to standard output
  | Inplace           -- ^ Edit file in place
  deriving (Eq, Show)


parseArgs :: IO Args
parseArgs = execParser $ info parser desc
  where desc = fullDesc
            <> progDesc "CLI interface to the Besra compiler"

parser :: Parser Args
parser = subparser fmtCommand
  where fmtCommand = command "fmt" $ info fmtParser fmtDesc
        fmtDesc = fullDesc
                <> header "besra fmt - a pretty printer/formatter for Besra files"
                <> progDesc "Formats input files."

fmtParser :: Parser Args
fmtParser = Fmt <$> fmtParser' where
  fmtParser' = FmtArgs <$> inputMode <*> outputMode
  inputMode = inputFile <|> stdInput
  inputFile = InputFile <$> argument str
    (metavar "FILE"
    <> help "Which FILE to read; if none provided, tries to read from stdin."
    )
  stdInput = flag' Stdin
    (long "stdin"
    <> help "Whether input should be read from standard input."
    )
  outputMode = checkMode <|> writeMode
  writeMode = WriteMode <$> flag Inplace Stdout
    (long "stdout"
    <> help ("Whether formatted output should be written to standard output "
          <> "or modified inplace. Defaults to inplace modification.")
    )
  checkMode = flag' CheckMode
    (long "check"
    <> help ("Whether check for already formatted file needs to be performed?"
          <> "If checking, exits with 0 if already formatted, otherwise 1.")
    )

